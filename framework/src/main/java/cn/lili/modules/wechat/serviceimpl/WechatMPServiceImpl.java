package cn.lili.modules.wechat.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.HttpUtils;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.enums.FulfillmentModeEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.service.WechatMPService;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 微信小程序 业务实现
 *
 * @author Bulbasaur
 */
@Service
@Slf4j
public class WechatMPServiceImpl implements WechatMPService {

    @Autowired
    private OrderService orderService;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private OrderPackageService orderPackageService;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;
    @Autowired
    private SettingService settingService;

    /**
     * 发货信息接口
     */
    String url = "https://api.weixin.qq.com/wxa/sec/order/upload_shipping_info?access_token=";
    /**
     * 物流公司列表
     */
    String DeliveryUrl = "https://api.weixin.qq.com/cgi-bin/express/delivery/open_msg/get_delivery_list?access_token=";
    /**
     * 是否开通发货信息管理
     */
    String isTradeManagedUrl = "https://api.weixin.qq.com/wxa/sec/order/is_trade_managed?access_token=";

    /**
     * 确认收货提醒接口
     */
    String confirmReceiveUrl = "https://api.weixin.qq.com/wxa/sec/order/notify_confirm_receive?access_token=";

    /**
     * 发货信息录入
     *
     * @param orderSn 订单号
     */
    @Override
    public void uploadShippingInfo(String orderSn) {

        Order order = orderService.getBySn(orderSn);
        //是否是微信小程序订单 && 微信支付
        if (!order.getClientType().equals(ClientTypeEnum.WECHAT_MP.name())
                || !order.getPaymentMethod().equals(PaymentMethodEnum.WECHAT.name())) {
            return;
        }
        //是否开通发货信息管理
        if (!isTradeManaged()) {
            return;
        }
        Setting systemSetting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
        WechatPaymentSetting wechatPaymentSetting = JSON.parseObject(systemSetting.getSettingValue(), WechatPaymentSetting.class);
        Map<String, Object> map = new HashMap<>(2);

        //发货信息录入
        //订单，需要上传物流信息的订单
        map.put("order_key", new OrderKey(order,wechatPaymentSetting.getMchId()));


        if (order.getOrderStatus().equals(OrderStatusEnum.TAKE.name())) {

            //物流模式，发货方式枚举值：1、实体物流配送采用快递公司进行实体物流配送形式 2、同城配送 3、虚拟商品，虚拟商品，例如话费充值，点卡等，无实体配送形式 4、用户自提
            map.put("logistics_type", 3);
            //发货模式，发货模式枚举值：1、UNIFIED_DELIVERY（统一发货）2、SPLIT_DELIVERY（分拆发货） 示例值: UNIFIED_DELIVERY
            map.put("delivery_mode", 1);


        } else if (order.getOrderStatus().equals(OrderStatusEnum.DELIVERED.name())) {

            List<OrderPackage> packages = orderPackageService.orderPackageList(order.getSn());
            boolean noDeliveryOnly = !packages.isEmpty() && packages.stream()
                    .allMatch(pkg -> FulfillmentModeEnum.NO_DELIVERY.name().equals(pkg.getFulfillmentMode()));
            List<OrderPackage> expressPackages = packages.stream()
                    .filter(pkg -> !FulfillmentModeEnum.NO_DELIVERY.name().equals(pkg.getFulfillmentMode())
                            && CharSequenceUtil.isNotBlank(pkg.getLogisticsNo()))
                    .toList();

            if (noDeliveryOnly) {
                map.put("logistics_type", 2);
                map.put("delivery_mode", 1);
                OrderPackage firstPackage = packages.get(0);
                List<Shipping> shippingList = new ArrayList<>();
                Shipping shipping = new Shipping();
                List<OrderItem> orderItemList = orderItemService.getByOrderSn(order.getSn());
                String itemDesc = CharSequenceUtil.isNotBlank(firstPackage.getDeliveryRemark())
                        ? firstPackage.getDeliveryRemark()
                        : orderItemList.get(0).getGoodsName();
                shipping.setItem_desc(itemDesc);
                Contact contact = new Contact();
                contact.setReceiver_contact(order.getConsigneeMobile());
                shipping.setContact(contact);
                shippingList.add(shipping);
                map.put("shipping_list", shippingList);
            } else if (expressPackages.size() > 1) {
                map.put("logistics_type", 1);
                map.put("delivery_mode", 2);
                List<Shipping> shippingList = new ArrayList<>();
                for (OrderPackage orderPackage : expressPackages) {
                    Shipping shipping = new Shipping();
                    shipping.setTracking_no(orderPackage.getLogisticsNo());
                    shipping.setExpress_company(getDeliveryList(orderPackage.getLogisticsName()));
                    List<OrderItem> orderItemList = orderItemService.getByOrderSn(order.getSn());
                    shipping.setItem_desc(orderItemList.get(0).getGoodsName());
                    Contact contact = new Contact();
                    contact.setReceiver_contact(order.getConsigneeMobile());
                    shipping.setContact(contact);
                    shippingList.add(shipping);
                }
                map.put("shipping_list", shippingList);
            } else {
                map.put("logistics_type", 1);
                map.put("delivery_mode", 1);
                List<Shipping> shippingList = new ArrayList<>();
                Shipping shipping = new Shipping();
                if (!expressPackages.isEmpty()) {
                    shipping.setTracking_no(expressPackages.get(0).getLogisticsNo());
                    shipping.setExpress_company(getDeliveryList(expressPackages.get(0).getLogisticsName()));
                } else {
                    shipping.setTracking_no(order.getLogisticsNo());
                    shipping.setExpress_company(getDeliveryList(order));
                }
                List<OrderItem> orderItemList = orderItemService.getByOrderSn(order.getSn());
                shipping.setItem_desc(orderItemList.get(0).getGoodsName());
                Contact contact = new Contact();
                contact.setReceiver_contact(order.getConsigneeMobile());
                shipping.setContact(contact);
                shippingList.add(shipping);
                map.put("shipping_list", shippingList);
            }


        } else if (order.getOrderStatus().equals(OrderStatusEnum.STAY_PICKED_UP.name())) {

            //物流模式，发货方式枚举值：1、实体物流配送采用快递公司进行实体物流配送形式 2、同城配送 3、虚拟商品，虚拟商品，例如话费充值，点卡等，无实体配送形式 4、用户自提
            map.put("logistics_type", 4);
            //发货模式，发货模式枚举值：1、UNIFIED_DELIVERY（统一发货）2、SPLIT_DELIVERY（分拆发货） 示例值: UNIFIED_DELIVERY
            map.put("delivery_mode", 1);

        }
        //上传时间，用于标识请求的先后顺序 示例值: `2022-12-15T13:29:35.120+08:00`
        map.put("upload_time", DateTimeFormatter.ISO_OFFSET_DATE_TIME.format(ZonedDateTime.now()));

        //支付者，支付者信息
        Connect connect = connectService.queryConnect(ConnectQueryDTO.builder().userId(order.getMemberId()).unionType(SourceEnum.WECHAT_MP_OPEN_ID.name()).build());
        if (connect == null) {
            return;
        }
        map.put("payer", new Payer(connect.getUnionId()));
        this.doPostWithJson(url, map);


    }

    /**
     * 订单确认收货
     * https://developers.weixin.qq.com/miniprogram/dev/platform-capabilities/business-capabilities/order-shipping/order-shipping.html#%E4%BA%94%E3%80%81%E7%A1%AE%E8%AE%A4%E6%94%B6%E8%B4%A7%E6%8F%90%E9%86%92%E6%8E%A5%E5%8F%A3
     *
     * @param orderSn 订单号
     */
    @Override
    public void notifyConfirmReceive(String orderSn) {
        Order order = orderService.getBySn(orderSn);

        //是否是微信小程序订单 && 微信支付
        if (!order.getClientType().equals(ClientTypeEnum.WECHAT_MP.name())
                || !order.getPaymentMethod().equals(PaymentMethodEnum.WECHAT.name())) {
            return;
        }
        //是否开通发货信息管理
        if (!isTradeManaged()) {
            return;
        }
        Map<String, Object> map = new HashMap<>(2);
        map.put("transaction_id", order.getReceivableNo());
        map.put("merchant_trade_no", order.getPayOrderNo());
        map.put("merchant_id", order.getPayOrderNo());

        Setting systemSetting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
        WechatPaymentSetting wechatPaymentSetting = JSON.parseObject(systemSetting.getSettingValue(), WechatPaymentSetting.class);
        map.put("merchant_id", wechatPaymentSetting.getMchId());
        //快递签收时间，时间戳形式
        // 微信要求的是秒级时间戳
        map.put("received_time", System.currentTimeMillis() / 1000);
        this.doPostWithJson(confirmReceiveUrl, map);
    }

    /**
     * 是否已开通发货信息管理服务
     * https://developers.weixin.qq.com/miniprogram/dev/platform-capabilities/business-capabilities/order-shipping/order-shipping.html#%E4%B8%83%E3%80%81%E6%9F%A5%E8%AF%A2%E5%B0%8F%E7%A8%8B%E5%BA%8F%E6%98%AF%E5%90%A6%E5%B7%B2%E5%BC%80%E9%80%9A%E5%8F%91%E8%B4%A7%E4%BF%A1%E6%81%AF%E7%AE%A1%E7%90%86%E6%9C%8D%E5%8A%A1
     */
    private Boolean isTradeManaged() {

        Setting systemSetting = settingService.get(SettingEnum.WECHAT_PAYMENT.name());
        WechatPaymentSetting wechatPaymentSetting = JSON.parseObject(systemSetting.getSettingValue(), WechatPaymentSetting.class);
        //发送url
        Map<String, Object> map = new HashMap<>(2);
        map.put("appid", wechatPaymentSetting.getMpAppId());
        JSONObject jsonObject = this.doPostWithJson(isTradeManagedUrl, map);
        return jsonObject.getBooleanValue("is_trade_managed");
    }

    /**
     * 查询物流公司编码列表
     * https://developers.weixin.qq.com/miniprogram/dev/platform-capabilities/industry/express/business/express_search.html#%E8%8E%B7%E5%8F%96%E8%BF%90%E5%8A%9Bid%E5%88%97%E8%A1%A8get-delivery-list
     */
    private String getDeliveryList(Order order) {
        return getDeliveryList(order.getLogisticsName());
    }

    private String getDeliveryList(String logisticsName) {
        Map<String, Object> map = new HashMap<>(2);
        JSONObject jsonObject = this.doPostWithJson(DeliveryUrl, map);
        List<Delivery> deliveryList = JSON.parseArray(jsonObject.getString("delivery_list"), Delivery.class);
        for (Delivery delivery : deliveryList) {
            if (logisticsName != null && logisticsName.equals(delivery.getDelivery_name())) {
                return delivery.getDelivery_id();
            }
        }
        throw new RuntimeException("未找到快递公司");
    }


    /**
     * 请求微信接口
     *
     * @param url 链接
     * @param map 参数
     * @return 返回内容
     */
    private JSONObject doPostWithJson(String url, Map map) {
        //获取token
        String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        //请求链接添加token
        url += token;
        //发起请求
        String content = HttpUtils.doPostWithJson(url, map);
        //记录请求结果
        log.info("微信小程序请求结果：" + content);
        //获取请求内容，如果token过期则重新获取，如果出错则抛出错误
        JSONObject jsonObject = JSON.parseObject(content);
        if (("0").equals(jsonObject.get("errcode").toString())) {
            return jsonObject;
        } else if (("40001").equals(jsonObject.get("errcode"))) {
            wechatAccessTokenUtil.removeAccessToken(ClientTypeEnum.WECHAT_MP);
            return this.doPostWithJson(url, map);
        } else {
            throw new ServiceException(jsonObject.get("errmsg").toString());
        }
    }


    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    class Contact {

        /**
         * 寄件人联系方式，寄件人联系方式，采用掩码传输，最后4位数字不能打掩码 示例值: `189****1234, 021-****1234, ****1234, 0**2-***1234, 0**2-******23-10, ****123-8008` 值限制: 0 ≤ value ≤ 1024
         */
        String consignor_contact;

        /**
         * 收件人联系方式，收件人联系方式为，采用掩码传输，最后4位数字不能打掩码 示例值: `189****1234, 021-****1234, ****1234, 0**2-***1234, 0**2-******23-10, ****123-8008` 值限制: 0 ≤ value ≤ 1024
         */
        String receiver_contact;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    class Delivery {

        private String delivery_id;
        private String delivery_name;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    class OrderKey {

        /**
         * 订单单号类型，用于确认需要上传详情的订单。枚举值1，使用下单商户号和商户侧单号；枚举值2，使用微信支付单号。
         */
        private Integer order_number_type;

        /**
         * 原支付交易对应的微信订单号
         */
        private String transaction_id;

        /**
         * 支付下单商户的商户号，由微信支付生成并下发
         */
        private String mchid;

        /**
         * 商户系统内部订单号，只能是数字、大小写字母`_-*`且在同一个商户号下唯一
         */
        private String out_trade_no;

        public OrderKey(Order order,String mchid) {
            this.order_number_type = 1;
            this.out_trade_no = order.getPayOrderNo();
            this.transaction_id = order.getReceivableNo();
            this.mchid = mchid;
        }
    }

    @Data
    @NoArgsConstructor
    class Payer {

        /**
         * 用户标识，用户在小程序appid下的唯一标识。 下单前需获取到用户的Openid 示例值: oUpF8uMuAJO_M2pxb1Q9zNjWeS6o 字符字节限制: [1, 128]
         */
        String openid;

        public Payer(String openid) {
            this.openid = openid;
        }
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    class Shipping {

        /**
         * 物流单号，物流快递发货时必填，示例值: 323244567777 字符字节限制: [1, 128]
         */
        private String tracking_no;

        /**
         * 物流公司编码，快递公司ID，参见「查询物流公司编码列表」，物流快递发货时必填， 示例值: DHL 字符字节限制: [1, 128]
         */
        private String express_company;

        /**
         * 商品信息，例如：微信红包抱枕*1个，限120个字以内
         */
        private String item_desc;

        /**
         * 联系方式，当发货的物流公司为顺丰时，联系方式为必填，收件人或寄件人联系方式二选一
         */
        private Contact contact;

    }

}
