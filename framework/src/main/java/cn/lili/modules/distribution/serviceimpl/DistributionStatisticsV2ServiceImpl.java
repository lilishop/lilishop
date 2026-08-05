package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.dos.DistributionShareEvent;
import cn.lili.modules.distribution.entity.dos.DistributionVisitEvent;
import cn.lili.modules.distribution.entity.dto.DistributionStatisticsQueryParams;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionBehaviorDataVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsDataVO;
import cn.lili.modules.distribution.entity.vos.DistributionOverviewV2VO;
import cn.lili.modules.distribution.mapper.DistributionShareEventMapper;
import cn.lili.modules.distribution.mapper.DistributionVisitEventMapper;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionStatisticsV2Service;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 分销统计V2服务实现
 *
 * @author pikachu
 */
@Service
public class DistributionStatisticsV2ServiceImpl implements DistributionStatisticsV2Service {

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionOrderService distributionOrderService;

    @Autowired
    private DistributionShareEventMapper shareEventMapper;

    @Autowired
    private DistributionVisitEventMapper visitEventMapper;

    @Autowired
    private MemberService memberService;

    @Override
    public DistributionOverviewV2VO overview(DistributionStatisticsQueryParams params) {
        Date start = params.getStartTime();
        Date end = params.getEndTime();
        Date todayStart = DateUtil.beginOfDay(new Date());
        Date todayEnd = DateUtil.endOfDay(new Date());

        DistributionOverviewV2VO vo = new DistributionOverviewV2VO();
        // 分销员总数与下级分销员数（含今日新增）
        vo.setTotalDistributorCount(distributionService.count(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getDistributionStatus, DistributionStatusEnum.PASS.name())));
        vo.setTotalSubDistributorCount(distributionService.count(new LambdaQueryWrapper<Distribution>()
                .isNotNull(Distribution::getParentDistributionId)));
        vo.setTodayNewDistributorCount(distributionService.count(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getDistributionStatus, DistributionStatusEnum.PASS.name())
                .between(Distribution::getJoinTime, todayStart, todayEnd)));
        vo.setTodayNewSubDistributorCount(distributionService.count(new LambdaQueryWrapper<Distribution>()
                .isNotNull(Distribution::getParentDistributionId)
                .between(Distribution::getJoinTime, todayStart, todayEnd)));
        // 累计佣金与今日佣金（扣除售后退回部分）
        vo.setTotalCommissionAmount(sumRewardAmount(null, null));
        vo.setTodayCommissionAmount(sumRewardAmount(todayStart, todayEnd));
        vo.setTotalShareVisitorCount(visitEventMapper.selectCount(new LambdaQueryWrapper<>()));
        return vo;
    }

    @Override
    public IPage<DistributionBehaviorDataVO> behaviorDataPage(DistributionStatisticsQueryParams params, PageVO page) {
        LambdaQueryWrapper<Distribution> wrapper = new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getDistributionStatus, DistributionStatusEnum.PASS.name());
        if (params.getDistributionId() != null) {
            wrapper.eq(Distribution::getId, params.getDistributionId());
        }
        IPage<Distribution> distributionPage = distributionService.page(PageUtil.initPage(page), wrapper);
        Page<DistributionBehaviorDataVO> result = new Page<>(distributionPage.getCurrent(), distributionPage.getSize(), distributionPage.getTotal());
        result.setRecords(distributionPage.getRecords().stream().map(this::toBehaviorVO).collect(Collectors.toList()));
        return result;
    }

    @Override
    public IPage<DistributionGoodsDataVO> goodsDataPage(DistributionStatisticsQueryParams params, PageVO page) {
        LambdaQueryWrapper<DistributionShareEvent> wrapper = new LambdaQueryWrapper<>();
        if (params.getGoodsId() != null) {
            wrapper.eq(DistributionShareEvent::getGoodsId, params.getGoodsId());
        }
        if (params.getStartTime() != null && params.getEndTime() != null) {
            wrapper.between(DistributionShareEvent::getShareTime, params.getStartTime(), params.getEndTime());
        }
        List<DistributionShareEvent> shareEvents = shareEventMapper.selectList(wrapper);
        // 内存按商品聚合分享事件，再手动分页
        Map<String, List<DistributionShareEvent>> grouped = shareEvents.stream()
                .filter(item -> item.getGoodsId() != null)
                .collect(Collectors.groupingBy(DistributionShareEvent::getGoodsId));
        List<DistributionGoodsDataVO> records = new ArrayList<>();
        for (Map.Entry<String, List<DistributionShareEvent>> entry : grouped.entrySet()) {
            records.add(toGoodsVO(entry.getKey(), entry.getValue()));
        }
        Page<DistributionGoodsDataVO> result = new Page<>(page.getPageNumber(), page.getPageSize(), records.size());
        int from = Math.min((page.getPageNumber() - 1) * page.getPageSize(), records.size());
        int to = Math.min(from + page.getPageSize(), records.size());
        result.setRecords(records.subList(from, to));
        return result;
    }

    /**
     * 将分销员实体转换为行为统计数据。
     *
     * @param distribution 分销员
     * @return 行为统计 VO
     */
    private DistributionBehaviorDataVO toBehaviorVO(Distribution distribution) {
        DistributionBehaviorDataVO vo = new DistributionBehaviorDataVO();
        vo.setDistributionId(distribution.getId());
        vo.setMemberId(distribution.getMemberId());
        vo.setNewSalesAmount(distribution.getValidSalesAmount());
        vo.setNewCustomerCount(distribution.getValidCustomerCount() == null ? 0L : distribution.getValidCustomerCount().longValue());
        vo.setNewInviteCount(distribution.getInviteCount() == null ? 0L : distribution.getInviteCount().longValue());
        // 分享次数与分享带来的访问人次
        vo.setShareCount(shareEventMapper.selectCount(new LambdaQueryWrapper<DistributionShareEvent>()
                .eq(DistributionShareEvent::getDistributionId, distribution.getId())));
        vo.setShareVisitorCount(visitEventMapper.selectCount(new LambdaQueryWrapper<DistributionVisitEvent>()
                .eq(DistributionVisitEvent::getDistributionId, distribution.getId())));
        Member member = memberService.getById(distribution.getMemberId());
        if (member != null) {
            vo.setNickname(member.getNickName());
            vo.setMobile(member.getMobile());
        }
        return vo;
    }

    /**
     * 汇总商品分享数据并构建商品统计 VO。
     *
     * @param goodsId     商品ID
     * @param shareEvents 分享事件列表
     * @return 商品统计 VO
     */
    private DistributionGoodsDataVO toGoodsVO(String goodsId, List<DistributionShareEvent> shareEvents) {
        DistributionGoodsDataVO vo = new DistributionGoodsDataVO();
        vo.setGoodsId(goodsId);
        vo.setGoodsName(null);
        vo.setShareCount((long) shareEvents.size());
        vo.setShareDistributorCount(shareEvents.stream()
                .map(DistributionShareEvent::getDistributionId).filter(Objects::nonNull).distinct().count());
        vo.setVisitVisitorCount(visitEventMapper.selectCount(new LambdaQueryWrapper<DistributionVisitEvent>()
                .eq(DistributionVisitEvent::getGoodsId, goodsId)));
        vo.setCommissionAmount(sumRewardAmountByGoods(goodsId));
        vo.setDealAmount(BigDecimal.ZERO);
        vo.setVisitConversionRate(BigDecimal.ZERO);
        return vo;
    }

    /**
     * 汇总奖励订单佣金总额。
     *
     * @param start 冻结时间起始（与 end 均为空则统计全部）
     * @param end   冻结时间结束
     * @return 佣金总额
     */
    private BigDecimal sumRewardAmount(Date start, Date end) {
        LambdaQueryWrapper<DistributionOrder> wrapper = new LambdaQueryWrapper<>();
        if (start != null && end != null) {
            wrapper.between(DistributionOrder::getCreateTime, start, end);
        }
        wrapper.ne(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.REFUND.name());
        return distributionOrderService.list(wrapper).stream()
                .map(order -> {
                    // 一级佣金扣除售后退回；二级邀请佣金同理
                    BigDecimal direct = BigDecimal.valueOf(order.getDirectCommission() == null ? 0D : order.getDirectCommission())
                            .subtract(BigDecimal.valueOf(order.getSellBackRebate() == null ? 0D : order.getSellBackRebate()));
                    BigDecimal invite = BigDecimal.valueOf(order.getInviteCommission() == null ? 0D : order.getInviteCommission())
                            .subtract(BigDecimal.valueOf(order.getInviteSellBackRebate() == null ? 0D : order.getInviteSellBackRebate()));
                    return direct.max(BigDecimal.ZERO).add(invite.max(BigDecimal.ZERO));
                })
                .reduce(BigDecimal.ZERO, BigDecimal::add);
    }

    /**
     * 按商品汇总佣金金额（当前返回零值占位）。
     *
     * @param goodsId 商品ID
     * @return 佣金总额
     */
    private BigDecimal sumRewardAmountByGoods(String goodsId) {
        return BigDecimal.ZERO;
    }
}
