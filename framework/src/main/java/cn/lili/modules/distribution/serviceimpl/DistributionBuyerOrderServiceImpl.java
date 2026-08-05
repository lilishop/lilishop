package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerOrderQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderSummaryVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderVO;
import cn.lili.modules.distribution.mapper.DistributionBuyerOrderMapper;
import cn.lili.modules.distribution.service.DistributionBuyerOrderService;
import cn.lili.modules.distribution.support.DistributionInviteRangeSupport;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * 分销员推广/邀请订单查询实现
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Service
public class DistributionBuyerOrderServiceImpl implements DistributionBuyerOrderService {

    private static final String ORDER_TYPE_INVITE = "INVITE";
    private static final String ORDER_TYPE_PROMOTION = "PROMOTION";

    @Autowired
    private DistributionBuyerOrderMapper buyerOrderMapper;

    /**
     * 分页查询推广/邀请订单及汇总佣金。
     *
     * @param distributionId 分销员ID
     * @param query          查询参数
     * @return 订单分页结果
     */
    @Override
    public DistributionBuyerOrderPageVO pageOrders(String distributionId, DistributionBuyerOrderQueryDTO query) {
        DistributionBuyerOrderQueryDTO safeQuery = query == null ? new DistributionBuyerOrderQueryDTO() : query;
        Page<?> page = PageUtil.initPage(safeQuery);
        long current = page.getCurrent();
        long size = page.getSize();
        String keyword = CharSequenceUtil.trim(safeQuery.getKeyword());
        var range = DistributionInviteRangeSupport.resolveRange(
                safeQuery.getRangeType(), safeQuery.getStartTime(), safeQuery.getEndTime());
        boolean inviteTab = isInviteOrderType(safeQuery.getOrderType());

        OrderPageSlice slice = loadOrderPage(distributionId, range, keyword, current, size, inviteTab);
        slice.records().forEach(this::fillSettlementText);

        DistributionBuyerOrderPageVO pageVO = new DistributionBuyerOrderPageVO();
        pageVO.setTotal(slice.total());
        pageVO.setCurrent(current);
        pageVO.setSize(size);
        pageVO.setTotalCommission(slice.totalCommission());
        pageVO.setRecords(slice.records());
        return pageVO;
    }

    /**
     * 查询推广/邀请订单详情。
     *
     * @param distributionId 分销员ID
     * @param orderItemSn    订单明细编号
     * @param orderType      订单类型 PROMOTION/INVITE
     * @return 订单详情
     */
    @Override
    public DistributionBuyerOrderDetailVO getOrderDetail(String distributionId, String orderItemSn, String orderType) {
        if (CharSequenceUtil.isBlank(orderItemSn)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "订单明细编号不能为空");
        }
        boolean inviteTab = isInviteOrderType(orderType);
        DistributionBuyerOrderDetailVO detail = inviteTab
                ? buyerOrderMapper.getInviteOrderDetail(distributionId, orderItemSn)
                : buyerOrderMapper.getPromotionOrderDetail(distributionId, orderItemSn);
        if (detail == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        fillDetailText(detail, inviteTab);
        return detail;
    }

    /**
     * 订单分页查询中间结果。
     *
     * @param total           总条数
     * @param totalCommission 总佣金
     * @param records         当前页记录
     */
    private record OrderPageSlice(long total, BigDecimal totalCommission, List<DistributionBuyerOrderVO> records) {
    }

    /**
     * 按订单类型加载分页数据与汇总佣金。
     *
     * @param distributionId 分销员 ID
     * @param range          下单时间范围
     * @param keyword        搜索关键词
     * @param current        当前页码
     * @param size           每页条数
     * @param inviteTab      是否为邀请订单
     * @return 分页切片
     */
    private OrderPageSlice loadOrderPage(String distributionId,
                                         DistributionInviteRangeSupport.DateRange range,
                                         String keyword,
                                         long current,
                                         long size,
                                         boolean inviteTab) {
        Date start = range.getStartTime();
        Date end = range.getEndTime();
        long offset = (current - 1) * size;

        // 先汇总总数与佣金，无数据则跳过分页查询
        DistributionBuyerOrderSummaryVO summary = inviteTab
                ? buyerOrderMapper.summarizeInviteOrders(distributionId, start, end, keyword)
                : buyerOrderMapper.summarizePromotionOrders(distributionId, start, end, keyword);

        long total = summary == null ? 0L : summary.getTotal();
        BigDecimal totalCommission = summary == null || summary.getTotalCommission() == null
                ? BigDecimal.ZERO : summary.getTotalCommission();
        if (total <= 0) {
            return new OrderPageSlice(total, totalCommission, List.of());
        }

        // 按推广/邀请类型分页拉取明细
        List<DistributionBuyerOrderVO> records = inviteTab
                ? buyerOrderMapper.listInviteOrders(distributionId, start, end, keyword, offset, size)
                : buyerOrderMapper.listPromotionOrders(distributionId, start, end, keyword, offset, size);
        return new OrderPageSlice(total, totalCommission, records);
    }

    /**
     * 判断是否为邀请订单类型。
     *
     * @param orderType 订单类型
     * @return 是否为 INVITE
     */
    private static boolean isInviteOrderType(String orderType) {
        return ORDER_TYPE_INVITE.equalsIgnoreCase(CharSequenceUtil.blankToDefault(orderType, ORDER_TYPE_PROMOTION));
    }

    /**
     * 填充订单详情的展示文案（类型、佣金标签、结算状态）。
     *
     * @param item      订单详情
     * @param inviteTab 是否为邀请订单
     */
    private void fillDetailText(DistributionBuyerOrderDetailVO item, boolean inviteTab) {
        fillSettlementText(item);
        item.setOrderType(inviteTab ? ORDER_TYPE_INVITE : ORDER_TYPE_PROMOTION);
        item.setCommissionLabel(inviteTab ? "邀请佣金" : "商品佣金");
        String type = CharSequenceUtil.blankToDefault(item.getSettlementStatusType(), "NOT_SETTLE");
        item.setSettlementStatusText(switch (type) {
            case "SETTLED" -> "已结算";
            case "PENDING" -> "待结算";
            default -> "不结算";
        });
    }

    /**
     * 填充分页订单的结算状态文案，退款时将佣金清零。
     *
     * @param item 订单列表项
     */
    private void fillSettlementText(DistributionBuyerOrderVO item) {
        if (item == null) {
            return;
        }
        if (item.getRefunded() == null) {
            item.setRefunded(false);
        }
        String type = CharSequenceUtil.blankToDefault(item.getSettlementStatusType(), "NOT_SETTLE");
        item.setSettlementStatusText(switch (type) {
            case "SETTLED" -> "已自动结算";
            case "PENDING" -> "待结算";
            default -> "不结算";
        });
        // 已退款订单展示时将佣金置零
        if (Boolean.TRUE.equals(item.getRefunded()) && item.getCommissionAmount() != null) {
            item.setCommissionAmount(BigDecimal.ZERO);
        }
    }
}
