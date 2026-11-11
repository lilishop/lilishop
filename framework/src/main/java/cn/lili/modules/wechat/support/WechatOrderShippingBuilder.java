package cn.lili.modules.wechat.support;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.function.Function;

/**
 * 将同笔微信支付下的已履约子订单归集成完整发货快照。
 * 部分发货暂不提交；混合配送类型必须人工核查，禁止以一个子订单覆盖整笔支付的物流。
 * 虚拟订单待核验、电子卡券交付及预约服务完成按各自真实履约节点处理。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Component
public class WechatOrderShippingBuilder {
    @Autowired
    private OrderItemService itemService;
    @Autowired
    private OrderPackageService packageService;

    /**
     * 生成未包含支付者、支付键及上传时间的发货快照；包裹按订单号和包裹号稳定排序。
     * @param orders 同笔支付的全部子订单
     * @param deliveryCode 按商城物流公司名称查询微信运力编码
     * @return 仅在整笔支付均满足履约条件时返回完整发货请求
     */
    public Map<String, Object> build(List<Order> orders, Function<String, String> deliveryCode) {
        if (orders.isEmpty()) throw failure("EMPTY_ORDER", "未找到支付关联订单");
        List<Order> sorted = orders.stream().sorted(Comparator.comparing(Order::getSn)).toList();
        Integer type = null;
        List<Map<String, Object>> shipping = new ArrayList<>();
        for (Order order : sorted) {
            int currentType = logisticsType(order);
            if (type != null && type != currentType) throw failure("MIXED_DELIVERY", "同笔支付含不同配送类型，请在微信后台核查处理");
            type = currentType;
            List<OrderItem> items = itemService.getByOrderSn(order.getSn());
            if (items == null || items.isEmpty()) throw failure("EMPTY_ITEMS", "订单商品信息缺失，请核查");
            String description = truncate(items.get(0).getGoodsName(), 110);
            if (description.isBlank()) throw failure("EMPTY_DESCRIPTION", "订单商品名称缺失，请核查");
            if (currentType == 1) {
                List<OrderPackage> packages = packages(order);
                if (packages.isEmpty()) {
                    shipping.add(express(order.getLogisticsNo(), order.getLogisticsName(), order.getConsigneeMobile(), description, deliveryCode));
                } else {
                    for (OrderPackage pkg : packages) {
                        if ("NO_DELIVERY".equals(pkg.getFulfillmentMode())) throw failure("MIXED_PACKAGES", "同笔订单含快递和无需配送包裹，请在微信后台核查处理");
                        shipping.add(express(pkg.getLogisticsNo(), pkg.getLogisticsName(),
                                pkg.getConsigneeMobile() == null ? order.getConsigneeMobile() : pkg.getConsigneeMobile(), description, deliveryCode));
                    }
                }
            } else {
                shipping.add(new LinkedHashMap<>(Map.of("item_desc", description)));
            }
        }
        if (type != 1) {
            String description = shipping.stream().map(s -> (String) s.get("item_desc")).reduce((a, b) -> a + "、" + b).orElse("");
            shipping = new ArrayList<>(List.of(new LinkedHashMap<>(Map.of("item_desc", truncate(description, 120)))));
        }
        if (shipping.size() > 10) throw failure("TOO_MANY_PACKAGES", "当前整笔支付超过十个包裹，请在微信后台核查处理");
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("logistics_type", type);
        result.put("delivery_mode", shipping.size() > 1 ? 2 : 1);
        if (shipping.size() > 1) result.put("is_all_delivered", true);
        result.put("shipping_list", shipping);
        return result;
    }

    /** 根据真实履约方式分类；未交付不能用支付成功或笼统完成状态伪造发货。 */
    private int logisticsType(Order order) {
        String status = order.getOrderStatus();
        if (!"PAID".equals(order.getPayStatus()) || "CANCELLED".equals(status)) {
            throw failure("INVALID_PAYMENT_STATE", "存在未支付或已关闭的关联订单，请核查支付及退款状态");
        }
        if ("APPOINTMENT".equals(order.getOrderType())) {
            if (!"COMPLETED".equals(status)) throw pending("WAIT_SERVICE", "预约服务尚未完成，暂不上报");
            return 3;
        }
        if ("SELF_PICK_UP".equals(order.getDeliveryMethod())) {
            if (!Set.of("STAY_PICKED_UP", "COMPLETED").contains(status)) throw pending("WAIT_PICKUP", "自提订单尚未备货，暂不上报");
            return 4;
        }
        if ("VIRTUAL".equals(order.getOrderType()) || "E_COUPON".equals(order.getOrderType()) || "VIRTUAL".equals(order.getDeliveryMethod())) {
            if (!Set.of("TAKE", "COMPLETED").contains(status)) throw pending("WAIT_VIRTUAL", "虚拟商品或电子卡券尚未交付，暂不上报");
            return 3;
        }
        if (!Set.of("DELIVERED", "COMPLETED").contains(status)) throw pending("WAIT_SHIPPING", "同笔支付尚有未完成发货的子订单，全部发货后上报");
        List<OrderPackage> packages = packages(order);
        if (!packages.isEmpty() && packages.stream().allMatch(p -> "NO_DELIVERY".equals(p.getFulfillmentMode()))) return 2;
        if ("LOCAL_TOWN_DELIVERY".equals(order.getDeliveryMethod())) return 2;
        return 1;
    }

    /** 包裹按稳定键排序；使用真实包裹快照，不从其他订单推断物流。 */
    private List<OrderPackage> packages(Order order) {
        List<OrderPackage> packages = packageService.orderPackageList(order.getSn());
        return packages == null ? List.of() : packages.stream()
                .sorted(Comparator.comparing(p -> Objects.toString(p.getPackageNo(), ""))).toList();
    }

    /** 校验快递单号及微信运力编码，手机掩码只保留最后四位。 */
    private Map<String, Object> express(String tracking, String company, String mobile, String description, Function<String, String> deliveryCode) {
        if (tracking == null || tracking.isBlank() || company == null || company.isBlank()) throw failure("MISSING_LOGISTICS", "快递单号或物流公司缺失，请先修正物流信息");
        String code = deliveryCode.apply(company);
        if (code == null || code.isBlank()) throw failure("UNKNOWN_LOGISTICS", "物流公司未匹配到微信运力编码");
        if (mobile == null || mobile.length() < 4) throw failure("MISSING_CONTACT", "收货联系方式缺失，请先修正收货信息");
        Map<String, Object> item = new LinkedHashMap<>();
        item.put("tracking_no", tracking);
        item.put("express_company", code);
        item.put("item_desc", description);
        item.put("contact", Map.of("receiver_contact", "****" + mobile.substring(mobile.length() - 4)));
        return item;
    }

    /** 按 Unicode 码点截断描述，避免拆断字符及超过微信字段长度。 */
    private String truncate(String value, int max) {
        if (value == null) return "";
        return value.substring(0, value.offsetByCodePoints(0, Math.min(max, value.codePointCount(0, value.length()))));
    }

    /** 构造需要修正数据或人工处理的失败，不允许继续调用微信。 */
    private WechatOrderSyncException failure(String code, String message) {
        return new WechatOrderSyncException(code, message, false);
    }

    /** 构造尚待真实履约完成的结果，保留待上报状态供后续事件处理。 */
    private WechatOrderSyncException pending(String code, String message) {
        return new WechatOrderSyncException(code, message, true);
    }
}
