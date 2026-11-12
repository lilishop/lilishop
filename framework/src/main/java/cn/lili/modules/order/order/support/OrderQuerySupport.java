package cn.lili.modules.order.order.support;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.live.entity.vos.LiveRoomVO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderExportDTO;
import cn.lili.modules.order.order.entity.dto.OrderExportDetailDTO;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.enums.OrderTypeEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderInfoVO;
import cn.lili.modules.order.order.entity.vo.OrderNumVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.entity.vo.PaymentLog;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.ReceiptService;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.mapper.TradeMapper;
import cn.lili.modules.order.trade.entity.dos.OrderLog;
import cn.lili.modules.order.trade.service.OrderLogService;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.*;

/**
 * 订单查询与导出支撑组件
 * <p>
 * 负责买家端/管理端订单列表查询与卡券凭证信息回填、数量统计、订单详情聚合装配、支付流水查询、直播间订单统计以及订单明细 Excel 导出。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderQuerySupport {

    @Autowired
    private PickupCardOrderService pickupCardOrderService;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private OrderLogService orderLogService;

    @Autowired
    private ReceiptService receiptService;

    @Autowired
    private TradeMapper tradeMapper;

    public OrderQuerySupport() {
    }

    public OrderQuerySupport(PickupCardOrderService pickupCardOrderService,
                             OrderItemService orderItemService,
                             OrderLogService orderLogService,
                             ReceiptService receiptService,
                             TradeMapper tradeMapper) {
        this.pickupCardOrderService = pickupCardOrderService;
        this.orderItemService = orderItemService;
        this.orderLogService = orderLogService;
        this.receiptService = receiptService;
        this.tradeMapper = tradeMapper;
    }

    /**
     * 构建订单通用列表查询包装器
     *
     * @param orderSearchParams 查询参数
     * @return 订单查询包装器
     */
    public QueryWrapper orderListWrapper(OrderSearchParams orderSearchParams) {
        QueryWrapper queryWrapper = orderSearchParams.queryWrapper();
        queryWrapper.groupBy("o.id");
        queryWrapper.orderByDesc("o.id");
        return queryWrapper;
    }

    /**
     * 为订单分页结果列表填充提货卡凭证信息
     *
     * @param page 分页结果
     * @return 填充卡券凭证后的订单简要信息分页
     */
    public IPage<OrderSimpleVO> fillPickupOrderInfo(IPage<OrderSimpleVO> page) {
        if (page == null || page.getRecords().isEmpty()) {
            return page;
        }
        Map<String, String> credentials = pickupCardOrderService.getCredentialIds(
                page.getRecords().stream().map(OrderSimpleVO::getSn).toList());
        page.getRecords().forEach(order -> order.setPickupCardId(credentials.get(order.getSn())));
        return page;
    }

    /**
     * 买家端订单简要信息分页查询
     *
     * @param orderSearchParams 查询参数
     * @param orderMapper       订单持久层映射器
     * @return 买家端订单简要信息分页
     */
    public IPage<OrderSimpleVO> queryBuyerByParams(OrderSearchParams orderSearchParams, OrderMapper orderMapper) {
        return fillPickupOrderInfo(orderMapper.queryBuyerByParams(PageUtil.initPage(orderSearchParams), orderListWrapper(orderSearchParams)));
    }

    /**
     * 管理端订单简要信息分页查询
     *
     * @param orderSearchParams 查询参数
     * @param orderMapper       订单持久层映射器
     * @return 管理端订单简要信息分页
     */
    public IPage<OrderSimpleVO> queryManageByParams(OrderSearchParams orderSearchParams, OrderMapper orderMapper) {
        return fillPickupOrderInfo(orderMapper.queryManageByParams(PageUtil.initPage(orderSearchParams), orderListWrapper(orderSearchParams)));
    }

    /**
     * 获取订单各类状态数量统计
     *
     * @param orderSearchParams 查询参数
     * @param orderMapper       订单持久层映射器
     * @return 订单数量统计视图对象
     */
    public OrderNumVO getOrderNumVO(OrderSearchParams orderSearchParams, OrderMapper orderMapper) {
        return orderMapper.getOrderNumVO(orderSearchParams.queryWrapper());
    }

    /**
     * 按条件查询订单列表
     *
     * @param orderSearchParams 查询参数
     * @param orderMapper       订单持久层映射器
     * @return 订单列表
     */
    public List<Order> queryListByParams(OrderSearchParams orderSearchParams, OrderMapper orderMapper) {
        return orderMapper.queryListByParams(orderSearchParams.queryWrapper());
    }

    /**
     * 按活动类型及支付状态等查询订单列表
     *
     * @param orderPromotionType 促销类型
     * @param payStatus          支付状态
     * @param parentOrderSn      父订单编号
     * @param orderSn            子订单编号
     * @param orderMapper        订单持久层映射器
     * @return 订单列表
     */
    public List<Order> queryListByPromotion(String orderPromotionType, String payStatus,
                                           String parentOrderSn, String orderSn, OrderMapper orderMapper) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getOrderPromotionType, orderPromotionType)
                .eq(Order::getPayStatus, payStatus)
                .and(i -> i.eq(Order::getParentOrderSn, parentOrderSn).or(j -> j.eq(Order::getSn, orderSn)));
        return orderMapper.selectList(queryWrapper);
    }

    /**
     * 按活动类型及支付状态等统计订单数量
     *
     * @param orderPromotionType 促销类型
     * @param payStatus          支付状态
     * @param parentOrderSn      父订单编号
     * @param orderSn            子订单编号
     * @param orderMapper        订单持久层映射器
     * @return 订单总数
     */
    public long queryCountByPromotion(String orderPromotionType, String payStatus,
                                      String parentOrderSn, String orderSn, OrderMapper orderMapper) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getOrderPromotionType, orderPromotionType)
                .eq(Order::getPayStatus, payStatus)
                .and(i -> i.eq(Order::getParentOrderSn, parentOrderSn).or(j -> j.eq(Order::getSn, orderSn)));
        return orderMapper.selectCount(queryWrapper);
    }

    /**
     * 根据拼团活动ID获取已支付拼团订单列表
     *
     * @param pintuanId   拼团活动ID
     * @param orderMapper 订单持久层映射器
     * @return 拼团订单列表
     */
    public List<Order> queryListByPromotion(String pintuanId, OrderMapper orderMapper) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Order::getOrderPromotionType, PromotionTypeEnum.PINTUAN.name());
        queryWrapper.eq(Order::getPromotionId, pintuanId);
        queryWrapper.nested(i -> i.eq(Order::getPayStatus, PayStatusEnum.PAID.name())
                .or(j -> j.eq(Order::getOrderStatus, OrderStatusEnum.PAID.name())));
        queryWrapper.ne(Order::getOrderStatus, OrderStatusEnum.CANCELLED.name());
        return orderMapper.selectList(queryWrapper);
    }

    /**
     * 导出订单数据至响应流
     *
     * @param response          HTTP 响应对象
     * @param orderSearchParams 查询参数
     * @param orderMapper       订单持久层映射器
     */
    public void queryExportOrder(HttpServletResponse response, OrderSearchParams orderSearchParams, OrderMapper orderMapper) {
        XSSFWorkbook workbook = initOrderExportData(orderMapper.queryExportOrder(orderSearchParams.queryWrapper()));
        try {
            String fileName = URLEncoder.encode("订单列表", StandardCharsets.UTF_8);
            response.setContentType("application/vnd.ms-excel;charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + fileName + ".xlsx");
            ServletOutputStream out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
            log.error("导出订单列表异常", e);
        } finally {
            try {
                workbook.close();
            } catch (Exception e) {
                log.error("订单列表关闭workbook异常", e);
            }
        }
    }

    /**
     * 查询订单全量聚合详情（包含商品项、日志、发票及提货凭证）
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     * @return 订单详情视图对象
     */
    public OrderDetailVO queryDetail(String orderSn, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        List<OrderItem> orderItems = orderItemService.getByOrderSn(orderSn);
        List<OrderLog> orderLogs = orderLogService.getOrderLog(orderSn);
        Receipt receipt = receiptService.getByOrderSn(orderSn);
        return new OrderDetailVO(new OrderInfoVO(order, pickupCardOrderService.getCredentialId(orderSn)),
                orderItems, orderLogs, receipt);
    }

    /**
     * 根据交易编号获取订单列表
     *
     * @param tradeSn     交易单号
     * @param orderMapper 订单持久层映射器
     * @return 订单列表
     */
    public List<Order> getByTradeSn(String tradeSn, OrderMapper orderMapper) {
        LambdaQueryWrapper<Order> queryWrapper = new LambdaQueryWrapper<>();
        return orderMapper.selectList(queryWrapper.eq(Order::getTradeSn, tradeSn));
    }

    /**
     * 获取订单应支付总额（支持从父交易单汇总读取）
     *
     * @param orderSn      订单编号
     * @param orderService 订单服务接口
     * @return 支付金额
     */
    public Double getPaymentTotal(String orderSn, OrderService orderService) {
        Order order = orderService.getBySn(orderSn);
        if (order == null) {
            Trade trade = getTradeBySn(orderSn);
            if (trade != null) {
                return trade.getFlowPrice();
            }
            return 0D;
        }
        Trade trade = getTradeBySn(order.getTradeSn());
        if (trade != null && CharSequenceUtil.isNotEmpty(trade.getPayStatus())
                && trade.getPayStatus().equals(PayStatusEnum.PAID.name())) {
            return trade.getFlowPrice();
        }
        return order.getFlowPrice();
    }

    private Trade getTradeBySn(String tradeSn) {
        if (CharSequenceUtil.isBlank(tradeSn) || tradeMapper == null) {
            return null;
        }
        return tradeMapper.selectOne(new LambdaQueryWrapper<Trade>().eq(Trade::getSn, tradeSn));
    }

    /**
     * 分页查询支付日志
     *
     * @param page         分页对象
     * @param queryWrapper 查询条件包装器
     * @param orderMapper  订单持久层映射器
     * @return 支付日志分页结果
     */
    public IPage<PaymentLog> queryPaymentLogs(IPage<PaymentLog> page, Wrapper<PaymentLog> queryWrapper, OrderMapper orderMapper) {
        return orderMapper.queryPaymentLogs(page, queryWrapper);
    }

    /**
     * 分页查询指定订单支付日志
     *
     * @param order       订单对象
     * @param searchVO    查询视图对象
     * @param pageVO      分页参数
     * @param orderMapper 订单持久层映射器
     * @return 支付日志分页
     */
    public IPage<PaymentLog> queryPaymentLogs(Order order, SearchVO searchVO, PageVO pageVO, OrderMapper orderMapper) {
        return queryPaymentLogs(PageUtil.initPage(pageVO), PageUtil.initWrapper(order, searchVO), orderMapper);
    }

    /**
     * 统计直播间订单销售金额及完成数量
     *
     * @param liveRoomVO   直播间视图数据
     * @param orderService 订单服务接口
     */
    public void countLiveOrderData(LiveRoomVO liveRoomVO, OrderService orderService) {
        if (liveRoomVO == null || CharSequenceUtil.isEmpty(liveRoomVO.getId())) {
            return;
        }
        List<Order> orderList = orderService.list(new LambdaQueryWrapper<Order>()
                .select(Order::getMemberId)
                .eq(Order::getLiveRoomId, liveRoomVO.getId())
                .eq(Order::getPayStatus, PayStatusEnum.PAID.name())
                .in(Order::getOrderStatus, OrderStatusEnum.PAID.name(), OrderStatusEnum.TAKE.name(), OrderStatusEnum.STAY_PICKED_UP.name(),
                        OrderStatusEnum.UNDELIVERED.name(), OrderStatusEnum.PARTS_DELIVERED.name(), OrderStatusEnum.DELIVERED.name(), OrderStatusEnum.COMPLETED.name()));

        if (orderList == null || orderList.isEmpty()) {
            liveRoomVO.setOrderCount(0);
            liveRoomVO.setOrderUserCount(0);
            return;
        }
        liveRoomVO.setOrderCount(orderList.size());
        int orderUserCount = (int) orderList.stream()
                .map(Order::getMemberId)
                .filter(CharSequenceUtil::isNotEmpty)
                .distinct()
                .count();
        liveRoomVO.setOrderUserCount(orderUserCount);
    }

    /**
     * 初始化构建订单导出 Excel 工作簿
     *
     * @param orderExportDTOList 导出的订单原始明细
     * @return Excel 工作簿对象
     */
    public XSSFWorkbook initOrderExportData(List<OrderExportDTO> orderExportDTOList) {
        List<OrderExportDetailDTO> orderExportDetailDTOList = new ArrayList<>();
        for (OrderExportDTO orderExportDTO : orderExportDTOList) {
            OrderExportDetailDTO orderExportDetailDTO = new OrderExportDetailDTO();
            BeanUtil.copyProperties(orderExportDTO, orderExportDetailDTO);

            PriceDetailDTO priceDetailDTO = com.alibaba.fastjson2.JSON.parseObject(orderExportDTO.getPriceDetail(), PriceDetailDTO.class);
            if (priceDetailDTO != null) {
                orderExportDetailDTO.setFreightPrice(priceDetailDTO.getFreightPrice());
                orderExportDetailDTO.setDiscountPrice(CurrencyUtil.add(priceDetailDTO.getDiscountPrice(), priceDetailDTO.getCouponPrice()));
                orderExportDetailDTO.setUpdatePrice(priceDetailDTO.getUpdatePrice());
                orderExportDetailDTO.setStoreMarketingCost(priceDetailDTO.getSiteCouponCommission());
                orderExportDetailDTO.setSiteMarketingCost(CurrencyUtil.sub(orderExportDetailDTO.getDiscountPrice(), orderExportDetailDTO.getStoreMarketingCost()));
            }

            if (StrUtil.isNotBlank(orderExportDTO.getConsigneeAddressPath())) {
                String[] receiveAddress = orderExportDTO.getConsigneeAddressPath().split(",");
                orderExportDetailDTO.setProvince(receiveAddress[0]);
                orderExportDetailDTO.setCity(receiveAddress.length > 1 ? receiveAddress[1] : "");
                orderExportDetailDTO.setDistrict(receiveAddress.length > 2 ? receiveAddress[2] : "");
                orderExportDetailDTO.setStreet(receiveAddress.length > 3 ? receiveAddress[3] : "");
            }

            orderExportDetailDTO.setOrderStatus(OrderStatusEnum.valueOf(orderExportDTO.getOrderStatus()).description());
            orderExportDetailDTO.setPaymentMethod(CharSequenceUtil.isNotBlank(orderExportDTO.getPaymentMethod())
                    ? PaymentMethodEnum.valueOf(orderExportDTO.getPaymentMethod()).paymentName() : "");
            orderExportDetailDTO.setClientType(ClientTypeEnum.valueOf(orderExportDTO.getClientType()).value());
            orderExportDetailDTO.setOrderType(OrderTypeEnum.NORMAL.name().equals(orderExportDTO.getOrderType()) ? "普通订单" : "虚拟订单");
            orderExportDetailDTO.setAfterSaleStatus(OrderItemAfterSaleStatusEnum.valueOf(orderExportDTO.getAfterSaleStatus()).description());

            orderExportDetailDTO.setCreateTime(DateUtil.formatDateTime(orderExportDTO.getCreateTime()));
            orderExportDetailDTO.setPaymentTime(DateUtil.formatDateTime(orderExportDTO.getPaymentTime()));
            orderExportDetailDTO.setLogisticsTime(DateUtil.formatDateTime(orderExportDTO.getLogisticsTime()));
            orderExportDetailDTO.setCompleteTime(DateUtil.formatDateTime(orderExportDTO.getCompleteTime()));
            orderExportDetailDTOList.add(orderExportDetailDTO);
        }

        XSSFWorkbook workbook = new XSSFWorkbook();
        Sheet sheet = workbook.createSheet("订单列表");

        Row header = sheet.createRow(0);
        String[] headers = {"订单号", "子订单号", "所选商品", "商品数量", "商品ID", "商品单价", "应付金额",
                "运费", "优惠总金额", "平台优惠", "商家优惠", "商家改价", "支付方式", "收件人", "收件人手机",
                "省", "市", "区", "街道", "详细地址", "备注", "订单提交时间", "支付时间", "来源",
                "订单状态", "订单类型", "售后状态", "取消原因", "发货时间", "完成时间", "店铺名称"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }

        for (int i = 0; i < orderExportDetailDTOList.size(); i++) {
            OrderExportDetailDTO dto = orderExportDetailDTOList.get(i);
            Row row = sheet.createRow(i + 1);
            row.createCell(0).setCellValue(dto.getOrderSn());
            row.createCell(1).setCellValue(dto.getOrderItemSn());
            row.createCell(2).setCellValue(dto.getGoodsName());
            row.createCell(3).setCellValue(dto.getNum() != null ? dto.getNum() : 0);
            row.createCell(4).setCellValue(dto.getGoodsId());
            row.createCell(5).setCellValue(Objects.nonNull(dto.getUnitPrice()) ? dto.getUnitPrice() : 0);
            row.createCell(6).setCellValue(Objects.nonNull(dto.getFlowPrice()) ? dto.getFlowPrice() : 0);
            row.createCell(7).setCellValue(Objects.nonNull(dto.getFreightPrice()) ? dto.getFreightPrice() : 0);
            row.createCell(8).setCellValue(Objects.nonNull(dto.getDiscountPrice()) ? dto.getDiscountPrice() : 0);
            row.createCell(9).setCellValue(Objects.nonNull(dto.getSiteMarketingCost()) ? dto.getSiteMarketingCost() : 0);
            row.createCell(10).setCellValue(Objects.nonNull(dto.getStoreMarketingCost()) ? dto.getStoreMarketingCost() : 0);
            row.createCell(11).setCellValue(Objects.nonNull(dto.getUpdatePrice()) ? dto.getUpdatePrice() : 0);
            row.createCell(12).setCellValue(dto.getPaymentMethod());
            row.createCell(13).setCellValue(dto.getConsigneeName());
            row.createCell(14).setCellValue(dto.getConsigneeMobile());
            row.createCell(15).setCellValue(dto.getProvince());
            row.createCell(16).setCellValue(dto.getCity());
            row.createCell(17).setCellValue(dto.getDistrict());
            row.createCell(18).setCellValue(dto.getStreet());
            row.createCell(19).setCellValue(dto.getConsigneeDetail());
            row.createCell(20).setCellValue(dto.getRemark());
            row.createCell(21).setCellValue(dto.getCreateTime());
            row.createCell(22).setCellValue(dto.getPaymentTime());
            row.createCell(23).setCellValue(dto.getClientType());
            row.createCell(24).setCellValue(dto.getOrderStatus());
            row.createCell(25).setCellValue(dto.getOrderType());
            row.createCell(26).setCellValue(dto.getAfterSaleStatus());
            row.createCell(27).setCellValue(dto.getCancelReason());
            row.createCell(28).setCellValue(dto.getLogisticsTime());
            row.createCell(29).setCellValue(dto.getCompleteTime());
            row.createCell(30).setCellValue(dto.getStoreName());
        }
        return workbook;
    }
}
