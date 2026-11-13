package cn.lili.modules.order.order.support;

import cn.hutool.core.io.IoUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dos.OrderPackageItem;
import cn.lili.modules.order.order.entity.dto.*;
import cn.lili.modules.order.order.entity.enums.*;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderPackageItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.store.entity.dto.StoreDeliverGoodsAddressDTO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.support.LogisticsGatewaySupport;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 订单发货与物流履约支撑组件
 * <p>
 * 负责整单发货、拆包发货、无需物流履约、批量发货模版导出与导入解析、顺丰寄件发货以及快递轨迹地图跟踪。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderDeliverySupport {

    private static final String ORDER_SN_COLUMN = "order_sn";
    private static final int MAX_DELIVERY_REMARK_LENGTH = 500;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderPackageService orderPackageService;

    @Autowired
    private OrderPackageItemService orderPackageItemService;

    @Autowired
    private LogisticsGatewaySupport logisticsGateway;

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private StoreDetailService storeDetailService;

    @Autowired
    private PickupCardOrderService pickupCardOrderService;

    public OrderDeliverySupport() {
    }

    /**
     * 注入订单履约所需存储服务、独立物流网关和订单来源查询。
     *
     * @param orderItemService 订单明细维护
     * @param orderPackageService 包裹维护
     * @param orderPackageItemService 包裹明细维护
     * @param logisticsGateway 物流基础查询和供应商调用
     * @param orderLogService 订单日志维护
     * @param storeDetailService 店铺寄件信息查询
     * @param pickupCardOrderService 提货卡订单来源查询
     */
    public OrderDeliverySupport(OrderItemService orderItemService,
                                OrderPackageService orderPackageService,
                                OrderPackageItemService orderPackageItemService,
                                LogisticsGatewaySupport logisticsGateway,
                                OrderLogService orderLogService,
                                StoreDetailService storeDetailService,
                                PickupCardOrderService pickupCardOrderService) {
        this.orderItemService = orderItemService;
        this.orderPackageService = orderPackageService;
        this.orderPackageItemService = orderPackageItemService;
        this.logisticsGateway = logisticsGateway;
        this.orderLogService = orderLogService;
        this.storeDetailService = storeDetailService;
        this.pickupCardOrderService = pickupCardOrderService;
    }

    /**
     * 普通整单发货
     *
     * @param orderSn      订单编号
     * @param logisticsNo  物流单号
     * @param logisticsId  物流公司ID
     * @param orderService 订单服务接口
     * @param orderMapper  订单持久层映射器
     * @return 发货后的订单实体
     */
    public Order delivery(String orderSn, String logisticsNo, String logisticsId,
                          OrderService orderService, OrderMapper orderMapper) {
        OrderFulfillParamsDTO params = new OrderFulfillParamsDTO();
        params.setOrderSn(orderSn);
        params.setFulfillmentMode(FulfillmentModeEnum.WHOLE.name());
        params.setLogisticsId(logisticsId);
        params.setLogisticsNo(logisticsNo);
        return fulfillOrder(params, orderService, orderMapper);
    }

    /**
     * 统一订单发货与履约处理
     *
     * @param params       发货履约参数
     * @param orderService 订单服务接口
     * @param orderMapper  订单持久层映射器
     * @return 履约后的订单实体
     */
    public Order fulfillOrder(OrderFulfillParamsDTO params, OrderService orderService, OrderMapper orderMapper) {
        if (params == null || CharSequenceUtil.isBlank(params.getOrderSn())) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        FulfillmentModeEnum mode = FulfillmentModeEnum.of(params.getFulfillmentMode());
        if (mode == null) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }

        String orderSn = params.getOrderSn();
        Order order = orderMapper.selectForUpdateBySn(orderSn);
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        OperationalJudgment.judgment(order);
        validateDeliverableOrder(order, mode);

        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        Map<String, OrderItem> itemMap = orderItems.stream()
                .collect(Collectors.toMap(OrderItem::getId, item -> item, (a, b) -> a));
        Map<String, Integer> deliveryLines = resolveDeliveryLines(mode, orderItems, params.getItems());
        if (deliveryLines.isEmpty()) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_NUM_ERROR);
        }

        Logistics logistics = null;
        if (mode == FulfillmentModeEnum.WHOLE || mode == FulfillmentModeEnum.SPLIT) {
            if (CharSequenceUtil.isBlank(params.getLogisticsId()) || CharSequenceUtil.isBlank(params.getLogisticsNo())) {
                throw new ServiceException(ResultCode.ORDER_LOGISTICS_ERROR);
            }
            logistics = logisticsGateway.getById(params.getLogisticsId());
            if (logistics == null) {
                throw new ServiceException(ResultCode.ORDER_LOGISTICS_ERROR);
            }
        } else if (CharSequenceUtil.isNotBlank(params.getLogisticsId()) || CharSequenceUtil.isNotBlank(params.getLogisticsNo())) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }

        String deliveryRemark = normalizeDeliveryRemark(params.getDeliveryRemark());
        Date now = new Date();

        OrderPackage orderPackage = new OrderPackage();
        orderPackage.setPackageNo(SnowFlake.createStr("OP"));
        orderPackage.setOrderSn(orderSn);
        orderPackage.setFulfillmentMode(mode.name());
        orderPackage.setDeliveryRemark(deliveryRemark);
        orderPackage.setLogisticsTime(now);
        orderPackage.setStatus("1");
        orderPackage.setConsigneeMobile(order.getConsigneeMobile());
        if (logistics != null) {
            orderPackage.setLogisticsNo(params.getLogisticsNo());
            orderPackage.setLogisticsCode(logistics.getId());
            orderPackage.setLogisticsName(logistics.getName());
        }
        orderPackageService.save(orderPackage);

        List<OrderLog> orderLogList = new ArrayList<>();
        for (Map.Entry<String, Integer> entry : deliveryLines.entrySet()) {
            OrderItem orderItem = itemMap.get(entry.getKey());
            if (orderItem == null) {
                throw new ServiceException(ResultCode.ORDER_DELIVER_NUM_ERROR);
            }
            int deliveryNum = entry.getValue();
            int remain = orderItem.getNum() - orderItem.getDeliverNumber();
            if (deliveryNum <= 0 || deliveryNum > remain) {
                throw new ServiceException(ResultCode.ORDER_DELIVER_NUM_ERROR);
            }
            orderItem.setDeliverNumber(orderItem.getDeliverNumber() + deliveryNum);

            OrderPackageItem orderPackageItem = new OrderPackageItem();
            orderPackageItem.setOrderSn(orderSn);
            orderPackageItem.setPackageNo(orderPackage.getPackageNo());
            orderPackageItem.setOrderItemSn(orderItem.getSn());
            orderPackageItem.setDeliverNumber(deliveryNum);
            orderPackageItem.setLogisticsTime(now);
            orderPackageItem.setGoodsName(orderItem.getGoodsName());
            orderPackageItem.setThumbnail(orderItem.getImage());
            orderPackageItemService.save(orderPackageItem);

            String logMessage = buildDeliveryLogMessage(orderSn, orderItem.getGoodsName(), deliveryNum, mode, orderPackage);
            orderLogList.add(new OrderLog(orderSn, UserContext.getCurrentUser().getId(),
                    UserContext.getCurrentUser().getRole().getRole(), UserContext.getCurrentUser().getUsername(), logMessage));
        }
        orderItemService.updateBatchById(orderItems);
        if (!orderLogList.isEmpty()) {
            orderLogService.saveBatch(orderLogList);
        }

        boolean allDelivered = orderItems.stream().allMatch(item -> item.getDeliverNumber() >= item.getNum());
        if (allDelivered) {
            finalizeOrderDelivered(order, orderPackage, mode, orderService);
        } else if (OrderStatusEnum.UNDELIVERED.name().equals(order.getOrderStatus())
                || DeliverStatusEnum.UNDELIVERED.name().equals(order.getDeliverStatus())) {
            order.setDeliverStatus(DeliverStatusEnum.PARTS_DELIVERED.name());
            order.setOrderStatus(OrderStatusEnum.PARTS_DELIVERED.name());
            orderService.updateById(order);
        }
        return order;
    }

    /**
     * 顺丰速运电子面单下单发货
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     * @param orderMapper  订单持久层映射器
     * @return 发货后的订单实体
     */
    public Order shunFengDelivery(String orderSn, OrderService orderService, OrderMapper orderMapper) {
        OrderDetailVO orderDetailVO = orderService.queryDetail(orderSn);
        String logisticsNo = logisticsGateway.sfCreateOrder(orderDetailVO);
        Logistics logistics = logisticsGateway.getByCode("SF");
        return delivery(orderSn, logisticsNo, logistics.getId(), orderService, orderMapper);
    }

    /**
     * 获取订单物流轨迹信息
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     * @return 快递轨迹信息
     */
    public Traces getTraces(String orderSn, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        return logisticsGateway.getLogisticTrack(order.getLogisticsCode(), order.getLogisticsNo(), order.getConsigneeMobile());
    }

    /**
     * 获取订单地图轨迹跟踪信息
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     * @return 地图物流轨迹
     */
    public Traces getMapTraces(String orderSn, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        StoreDeliverGoodsAddressDTO storeDeliverGoodsAddressDTO = storeDetailService.getStoreDeliverGoodsAddressDto(order.getStoreId());
        String from = storeDeliverGoodsAddressDTO.getSalesConsignorAddressPath().substring(0,
                storeDeliverGoodsAddressDTO.getSalesConsignorAddressPath().indexOf(",") - 1);
        String to = order.getConsigneeAddressPath().substring(0, order.getConsigneeAddressPath().indexOf(",") - 1);
        return logisticsGateway.getLogisticMapTrack(order.getLogisticsCode(), order.getLogisticsNo(), order.getConsigneeMobile(), from, to);
    }

    /**
     * 导出批量发货 Excel 模板
     *
     * @param response      HTTP 响应
     * @param logisticsName 可选物流公司名称列表
     */
    public void getBatchDeliverList(HttpServletResponse response, List<String> logisticsName) {
        ExcelWriter writer = ExcelUtil.getWriter();
        ArrayList<String> rows = new ArrayList<>();
        rows.add("订单号");
        rows.add("物流公司");
        rows.add("物流单号");
        writer.writeHeadRow(rows);

        String[] logiList = logisticsName.toArray(new String[]{});
        CellRangeAddressList cellRangeAddressList = new CellRangeAddressList(1, 200, 1, 1);
        writer.addSelect(cellRangeAddressList, logiList);

        ServletOutputStream out = null;
        try {
            response.setContentType("application/vnd.ms-excel;charset=utf-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + URLEncoder.encode("发货单模版", StandardCharsets.UTF_8) + ".xls");
            out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("获取发货单模版错误", e);
        } finally {
            writer.close();
            IoUtil.close(out);
        }
    }

    /**
     * Excel 导入批量发货
     *
     * @param files        上传的 Excel 文件
     * @param orderService 订单服务接口
     * @param orderMapper  订单持久层映射器
     */
    public void batchDeliver(MultipartFile files, OrderService orderService, OrderMapper orderMapper) {
        InputStream inputStream;
        List<OrderBatchDeliverDTO> orderBatchDeliverDTOList = new ArrayList<>();
        try {
            inputStream = files.getInputStream();
            ExcelReader excelReader = ExcelUtil.getReader(inputStream);
            List<List<Object>> read = excelReader.read(1, excelReader.getRowCount());
            for (List<Object> objects : read) {
                OrderBatchDeliverDTO orderBatchDeliverDTO = new OrderBatchDeliverDTO();
                orderBatchDeliverDTO.setOrderSn(objects.get(0).toString());
                orderBatchDeliverDTO.setLogisticsName(objects.get(1).toString());
                orderBatchDeliverDTO.setLogisticsNo(objects.get(2).toString());
                orderBatchDeliverDTOList.add(orderBatchDeliverDTO);
            }
        } catch (Exception e) {
            throw new ServiceException(ResultCode.ORDER_BATCH_DELIVER_ERROR);
        }
        checkBatchDeliver(orderBatchDeliverDTOList, orderService);
        for (OrderBatchDeliverDTO orderBatchDeliverDTO : orderBatchDeliverDTOList) {
            orderService.delivery(orderBatchDeliverDTO.getOrderSn(), orderBatchDeliverDTO.getLogisticsNo(), orderBatchDeliverDTO.getLogisticsId());
        }
    }

    /**
     * 校验待批量发货的数据列表合法性
     *
     * @param list         批量发货参数列表
     * @param orderService 订单服务接口
     */
    public void checkBatchDeliver(List<OrderBatchDeliverDTO> list, OrderService orderService) {
        Map<String, String> logisticsMap = logisticsGateway.list().stream()
                .collect(Collectors.toMap(Logistics::getName, Logistics::getId));
        for (OrderBatchDeliverDTO orderBatchDeliverDTO : list) {
            Order order = orderService.getOne(new LambdaQueryWrapper<Order>()
                    .eq(Order::getStoreId, UserContext.getCurrentUser().getStoreId())
                    .eq(Order::getSn, orderBatchDeliverDTO.getOrderSn()));
            if (order == null) {
                throw new ServiceException("订单号：'" + orderBatchDeliverDTO.getOrderSn() + " '不存在");
            } else if (!order.getOrderStatus().equals(OrderStatusEnum.UNDELIVERED.name())) {
                throw new ServiceException("订单号：'" + orderBatchDeliverDTO.getOrderSn() + " '不能发货");
            }
            String logisticsId = logisticsMap.get(orderBatchDeliverDTO.getLogisticsName());
            if (logisticsId != null) {
                orderBatchDeliverDTO.setLogisticsId(logisticsId);
            }
            if (CharSequenceUtil.isEmpty(orderBatchDeliverDTO.getLogisticsId())) {
                throw new ServiceException("物流公司：'" + orderBatchDeliverDTO.getLogisticsName() + " '不存在");
            }
        }
    }

    /**
     * 部分拆包发货参数转换适配
     *
     * @param partDeliveryParamsDTO 拆单发货参数
     * @param orderService          订单服务接口
     * @param orderMapper           订单持久层映射器
     * @return 发货后的订单
     */
    public Order partDelivery(PartDeliveryParamsDTO partDeliveryParamsDTO, OrderService orderService, OrderMapper orderMapper) {
        if (partDeliveryParamsDTO == null || CharSequenceUtil.isBlank(partDeliveryParamsDTO.getOrderSn())) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        OrderFulfillParamsDTO params = new OrderFulfillParamsDTO();
        params.setOrderSn(partDeliveryParamsDTO.getOrderSn());
        params.setFulfillmentMode(FulfillmentModeEnum.SPLIT.name());
        params.setLogisticsId(partDeliveryParamsDTO.getLogisticsId());
        params.setLogisticsNo(partDeliveryParamsDTO.getLogisticsNo());
        if (partDeliveryParamsDTO.getPartDeliveryDTOList() != null) {
            List<OrderFulfillItemDTO> items = new ArrayList<>();
            for (PartDeliveryDTO partDeliveryDTO : partDeliveryParamsDTO.getPartDeliveryDTOList()) {
                OrderFulfillItemDTO item = new OrderFulfillItemDTO();
                item.setOrderItemId(partDeliveryDTO.getOrderItemId());
                item.setDeliveryNum(partDeliveryDTO.getDeliveryNum());
                items.add(item);
            }
            params.setItems(items);
        }
        return fulfillOrder(params, orderService, orderMapper);
    }

    /**
     * 校验订单是否具备发货前置状态
     *
     * @param order 订单实体
     * @param mode  履约模式
     */
    public void validateDeliverableOrder(Order order, FulfillmentModeEnum mode) {
        if (!OrderTypeEnum.NORMAL.name().equals(order.getOrderType())) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        if (DeliveryMethodEnum.SELF_PICK_UP.name().equals(order.getDeliveryMethod())) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        boolean deliverableStatus = (DeliverStatusEnum.UNDELIVERED.name().equals(order.getDeliverStatus())
                || DeliverStatusEnum.PARTS_DELIVERED.name().equals(order.getDeliverStatus()))
                && (OrderStatusEnum.UNDELIVERED.name().equals(order.getOrderStatus())
                || OrderStatusEnum.PARTS_DELIVERED.name().equals(order.getOrderStatus()));
        if (!deliverableStatus) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        if (OrderStatusEnum.PARTS_DELIVERED.name().equals(order.getOrderStatus())
                || DeliverStatusEnum.PARTS_DELIVERED.name().equals(order.getDeliverStatus())) {
            if (mode != FulfillmentModeEnum.SPLIT) {
                throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
            }
        }
        if ((mode == FulfillmentModeEnum.WHOLE || mode == FulfillmentModeEnum.NO_DELIVERY)
                && !OrderStatusEnum.UNDELIVERED.name().equals(order.getOrderStatus())) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
    }

    /**
     * 计算解析各发货商品项的数量明细
     *
     * @param mode         履约模式
     * @param orderItems   订单项列表
     * @param requestItems 请求发货项
     * @return 订单项ID与本次发货数量映射
     */
    public Map<String, Integer> resolveDeliveryLines(FulfillmentModeEnum mode, List<OrderItem> orderItems,
                                                     List<OrderFulfillItemDTO> requestItems) {
        Map<String, Integer> lines = new LinkedHashMap<>();
        if (mode == FulfillmentModeEnum.WHOLE || mode == FulfillmentModeEnum.NO_DELIVERY) {
            for (OrderItem orderItem : orderItems) {
                int remain = orderItem.getNum() - orderItem.getDeliverNumber();
                if (remain > 0) {
                    lines.put(orderItem.getId(), remain);
                }
            }
            return lines;
        }
        if (requestItems == null || requestItems.isEmpty()) {
            return lines;
        }
        for (OrderFulfillItemDTO requestItem : requestItems) {
            if (requestItem == null || CharSequenceUtil.isBlank(requestItem.getOrderItemId())) {
                continue;
            }
            OrderItem matched = orderItems.stream()
                    .filter(item -> requestItem.getOrderItemId().equals(item.getId()))
                    .findFirst()
                    .orElse(null);
            if (matched == null) {
                throw new ServiceException(ResultCode.ORDER_DELIVER_NUM_ERROR);
            }
            int remain = matched.getNum() - matched.getDeliverNumber();
            int deliveryNum = requestItem.getDeliveryNum() == null ? remain : requestItem.getDeliveryNum();
            if (deliveryNum <= 0 || deliveryNum > remain) {
                throw new ServiceException(ResultCode.ORDER_DELIVER_NUM_ERROR);
            }
            lines.merge(requestItem.getOrderItemId(), deliveryNum, Integer::sum);
            if (lines.get(requestItem.getOrderItemId()) > remain) {
                throw new ServiceException(ResultCode.ORDER_DELIVER_NUM_ERROR);
            }
        }
        return lines;
    }

    /**
     * 规范化处理发货备注信息
     *
     * @param deliveryRemark 原始发货备注
     * @return 规范化截取后备注
     */
    public String normalizeDeliveryRemark(String deliveryRemark) {
        if (CharSequenceUtil.isBlank(deliveryRemark)) {
            return null;
        }
        String trimmed = deliveryRemark.trim();
        if (trimmed.length() > MAX_DELIVERY_REMARK_LENGTH) {
            throw new ServiceException(ResultCode.ORDER_DELIVER_ERROR);
        }
        return trimmed;
    }

    /**
     * 组装发货日志文本信息
     *
     * @param orderSn      订单编号
     * @param goodsName    商品名称
     * @param deliveryNum  发货数量
     * @param mode         履约模式
     * @param orderPackage 发货包裹
     * @return 日志文本
     */
    public String buildDeliveryLogMessage(String orderSn, String goodsName, int deliveryNum,
                                          FulfillmentModeEnum mode, OrderPackage orderPackage) {
        if (mode == FulfillmentModeEnum.NO_DELIVERY) {
            return "订单 [ " + orderSn + " ] 商品 [ " + goodsName + " ] 无需物流发货 [ " + deliveryNum + " ]"
                    + (CharSequenceUtil.isNotBlank(orderPackage.getDeliveryRemark())
                    ? "，发货备注 [ " + orderPackage.getDeliveryRemark() + " ]" : "");
        }
        return "订单 [ " + orderSn + " ] 商品 [ " + goodsName + " ] 发货 [ " + deliveryNum + " ]，发货单号 [ "
                + orderPackage.getLogisticsNo() + " ]";
    }

    /**
     * 订单全部商品项发货完成时的收尾状态流转与通知
     *
     * @param order        订单实体
     * @param orderPackage 最新发货包裹
     * @param mode         履约模式
     * @param orderService 订单服务接口
     */
    public void finalizeOrderDelivered(Order order, OrderPackage orderPackage,
                                       FulfillmentModeEnum mode, OrderService orderService) {
        if (mode == FulfillmentModeEnum.NO_DELIVERY) {
            order.setLogisticsCode(null);
            order.setLogisticsName(null);
            order.setLogisticsNo(null);
        } else {
            order.setLogisticsCode(orderPackage.getLogisticsCode());
            order.setLogisticsName(orderPackage.getLogisticsName());
            order.setLogisticsNo(orderPackage.getLogisticsNo());
        }
        order.setLogisticsTime(orderPackage.getLogisticsTime());
        order.setDeliverStatus(DeliverStatusEnum.DELIVERED.name());
        orderService.updateById(order);
        orderMapperUpdateStatus(order.getSn(), OrderStatusEnum.DELIVERED, orderService);
        orderItemService.update(new UpdateWrapper<OrderItem>().eq(ORDER_SN_COLUMN, order.getSn())
                .set("after_sale_status", !pickupCardOrderService.isPickupOrder(order.getSn()) ? OrderItemAfterSaleStatusEnum.NOT_APPLIED : OrderItemAfterSaleStatusEnum.EXPIRED)
                .set("complain_status", OrderComplaintStatusEnum.NO_APPLY));
        OrderMessage orderMessage = new OrderMessage();
        orderMessage.setNewStatus(OrderStatusEnum.DELIVERED);
        orderMessage.setOrderSn(order.getSn());
        orderService.sendUpdateStatusMessage(orderMessage);
    }

    private void orderMapperUpdateStatus(String orderSn, OrderStatusEnum orderStatus, OrderService orderService) {
        orderService.update(new com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper<Order>()
                .eq(Order::getSn, orderSn)
                .set(Order::getOrderStatus, orderStatus.name()));
    }
}
