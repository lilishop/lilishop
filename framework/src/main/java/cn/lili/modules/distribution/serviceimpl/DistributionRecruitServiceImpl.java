package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyFormItemSnapshot;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.enums.RecruitApplyModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitAuditModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitJoinConditionTypeEnum;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitConditionProgressVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitPlanGoodsItemVO;
import cn.lili.modules.distribution.mapper.DistributionRecruitApplicationMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.distribution.support.DistributionMemberSupport;
import cn.lili.modules.distribution.support.DistributionParentBindSupport;
import cn.lili.modules.distribution.support.DistributionRecruitSettingSupport;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dto.DistributionRecruitFieldSetting;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 分销招募服务实现
 *
 * @author pikachu
 */
@Service
public class DistributionRecruitServiceImpl implements DistributionRecruitService {

    @Autowired
    private DistributionRecruitSettingSupport recruitSettingSupport;

    @Autowired
    private DistributionRecruitApplicationMapper applicationMapper;

    @Autowired
    private DistributionMemberSupport distributionMemberSupport;

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    @Autowired
    private DistributionParentBindSupport distributionParentBindSupport;

    @Autowired
    private MemberService memberService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DistributionRecruitSetting saveSetting(DistributionRecruitSetting setting) {
        prepareSettingForSave(setting);
        DistributionRecruitSetting saved = recruitSettingSupport.saveSetting(setting);
        enrichRecruitGoods(saved);
        return saved;
    }

    @Override
    public DistributionRecruitSetting getSetting() {
        DistributionRecruitSetting setting = recruitSettingSupport.readSetting();
        enrichRecruitGoods(setting);
        return setting;
    }

    @Override
    public DistributionRecruitPageVO getRecruitPage() {
        DistributionRecruitSetting setting = getSetting();
        DistributionRecruitPageVO pageVO = new DistributionRecruitPageVO();
        // planVersion > 0 表示已配置招募方案
        boolean configured = setting.getPlanVersion() != null && setting.getPlanVersion() > 0;
        pageVO.setPlan(configured ? setting : null);
        pageVO.setOnlineEntryVisible(configured && Boolean.TRUE.equals(setting.getOnlineEntryVisible()));
        pageVO.setFields(configured && setting.getFields() != null ? setting.getFields() : Collections.emptyList());
        // 已登录用户补充分销状态与最近申请记录
        if (UserContext.getCurrentUser() != null) {
            String memberId = UserContext.getCurrentUser().getId();
            Distribution distribution = distributionMemberSupport.getByMemberId(memberId);
            if (distribution != null) {
                pageVO.setDistributionStatus(distribution.getDistributionStatus());
            }
            DistributionRecruitApplication application = applicationMapper.selectOne(
                    new LambdaQueryWrapper<DistributionRecruitApplication>()
                            .eq(DistributionRecruitApplication::getMemberId, memberId)
                            .orderByDesc(DistributionRecruitApplication::getCreateTime)
                            .last("limit 1"));
            pageVO.setCurrentApplication(application);
        }
        return pageVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public DistributionRecruitApplication submitApplication(DistributionRecruitApplyDTO applyDTO) {
        DistributionRecruitSetting setting = getSetting();
        if (setting.getPlanVersion() == null || setting.getPlanVersion() <= 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "暂无分销招募设置");
        }
        String memberId = UserContext.getCurrentUser().getId();
        // 已是分销员或存在阻塞性申请状态时拒绝重复提交
        Distribution existing = distributionMemberSupport.getByMemberId(memberId);
        if (distributionMemberSupport.isBlockingRecruitApply(existing)) {
            throw new ServiceException(ResultCode.DISTRIBUTION_IS_APPLY);
        }
        if (getPendingApplicationByMemberId(memberId) != null) {
            throw new ServiceException(ResultCode.DISTRIBUTION_IS_APPLY);
        }
        DistributionRecruitApplication application = new DistributionRecruitApplication();
        application.setMemberId(memberId);
        application.setPlanId(DistributionRecruitSettingSupport.SETTING_KEY);
        application.setPlanVersion(setting.getPlanVersion());
        application.setApplySource(RecruitApplyModeEnum.AUTO.name().equals(setting.getApplyMode())
                ? "AUTO_ENTRY" : "MANUAL_ENTRY");
        application.setAuditMode(setting.getAuditMode());
        application.setRecruitToken(applyDTO.getRecruitToken());
        if (setting.getAgreementContent() != null && !setting.getAgreementContent().isBlank()) {
            if (!Boolean.TRUE.equals(applyDTO.getAgreementAccepted())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "请先同意分销推广服务协议");
            }
            application.setAgreementContent(setting.getAgreementContent());
            application.setAgreementAcceptTime(new Date());
        }
        DistributionRecruitConditionProgressVO conditionProgress = getConditionProgress(memberId);
        if (RecruitJoinConditionTypeEnum.CONDITIONAL.name().equals(setting.getJoinConditionType())
                && !Boolean.TRUE.equals(conditionProgress.getSatisfied())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "尚未满足加入条件");
        }
        application.setConditionSnapshot(JSONUtil.toJsonStr(conditionProgress));
        application.setApplyFormSnapshot(buildApplyFormSnapshot(setting, applyDTO.getFormItems()));
        // 记录当前绑定的推广分销员，审核通过时用于确定上级归属
        DistributionCustomerRelation relation = customerRelationService.getActiveRelation(memberId);
        application.setBoundDistributorId(relation == null ? null : relation.getDistributionId());
        application.setApplicationStatus("PENDING");
        // 人工审核需先创建待审核分销员记录
        if (RecruitAuditModeEnum.MANUAL.name().equals(setting.getAuditMode())) {
            Member member = memberService.getById(memberId);
            Distribution pendingDistribution = distributionMemberSupport.markAsPendingApply(
                    memberId, member == null ? "" : member.getNickName());
            application.setDistributionId(pendingDistribution.getId());
        }
        applicationMapper.insert(application);
        // 自动审核模式：插入后立即走审核通过流程
        if (RecruitAuditModeEnum.AUTO_PASS.name().equals(setting.getAuditMode())) {
            auditApplication(application.getId(), true, "自动审核通过", "system");
            application = applicationMapper.selectById(application.getId());
        }
        return application;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void auditApplication(String applicationId, boolean pass, String remark, String operatorId) {
        DistributionRecruitApplication application = applicationMapper.selectById(applicationId);
        if (application == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR);
        }
        application.setAuditUserId(operatorId);
        application.setAuditTime(new Date());
        application.setAuditRemark(remark);
        if (!pass) {
            application.setApplicationStatus("REJECTED");
            application.setRejectReason(remark);
            applicationMapper.updateById(application);
            // 拒绝时同步将待审核分销员标记为 REFUSE
            if (application.getDistributionId() != null) {
                Distribution distribution = distributionMemberSupport.getById(application.getDistributionId());
                if (distribution != null
                        && DistributionStatusEnum.APPLY.name().equals(distribution.getDistributionStatus())) {
                    distribution.setDistributionStatus(DistributionStatusEnum.REFUSE.name());
                    distributionMemberSupport.updateById(distribution);
                }
            }
            return;
        }
        application.setApplicationStatus("APPROVED");
        applicationMapper.updateById(application);
        // 激活分销员身份（自动审核场景下可能新建记录）
        Member member = memberService.getById(application.getMemberId());
        Distribution distribution = distributionMemberSupport.activateOnRecruitPass(
                application.getMemberId(), member == null ? "" : member.getNickName());
        if (!distribution.getId().equals(application.getDistributionId())) {
            application.setDistributionId(distribution.getId());
            applicationMapper.updateById(application);
        }
        distributionParentBindSupport.bindParentOnAuditPass(distribution.getId(),
                application.getRecruitInviterId(), application.getBoundDistributorId());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void tryAutoApply(String memberId) {
        DistributionRecruitSetting setting = getSetting();
        if (!RecruitApplyModeEnum.AUTO.name().equals(setting.getApplyMode())) {
            return;
        }
        // 仅当加入条件已满足时才触发自动申请
        DistributionRecruitConditionProgressVO progress = getConditionProgress(memberId);
        if (!Boolean.TRUE.equals(progress.getSatisfied())) {
            return;
        }
        submitApplication(new DistributionRecruitApplyDTO());
    }

    @Override
    public DistributionRecruitConditionProgressVO getConditionProgress(String memberId) {
        DistributionRecruitSetting setting = recruitSettingSupport.readSetting();
        DistributionRecruitConditionProgressVO progress = new DistributionRecruitConditionProgressVO();
        // 未配置招募方案时不设门槛
        if (setting.getPlanVersion() == null || setting.getPlanVersion() <= 0) {
            progress.setSatisfied(true);
            return progress;
        }
        progress.setJoinConditionType(setting.getJoinConditionType());
        progress.setMinSelfPurchaseAmount(setting.getMinSelfPurchaseAmount());
        progress.setMinConsumeCount(setting.getMinConsumeCount());
        if (!RecruitJoinConditionTypeEnum.CONDITIONAL.name().equals(setting.getJoinConditionType()) || memberId == null) {
            progress.setSatisfied(!RecruitJoinConditionTypeEnum.CONDITIONAL.name().equals(setting.getJoinConditionType()));
            return progress;
        }
        boolean requirePurchaseGoods = Boolean.TRUE.equals(setting.getRequirePurchaseGoods());
        boolean requireSelfPurchaseAmount = Boolean.TRUE.equals(setting.getRequireSelfPurchaseAmount());
        boolean requireConsumeCount = Boolean.TRUE.equals(setting.getRequireConsumeCount());
        if (!requirePurchaseGoods && !requireSelfPurchaseAmount && !requireConsumeCount) {
            progress.setSatisfied(true);
            return progress;
        }
        // 统计已支付订单作为自购金额与消费次数依据
        List<Order> paidOrders = orderService.list(new LambdaQueryWrapper<Order>()
                .eq(Order::getMemberId, memberId)
                .eq(Order::getPayStatus, PayStatusEnum.PAID.name()));
        BigDecimal selfPurchaseAmount = paidOrders.stream()
                .map(item -> item.getFlowPrice() == null ? BigDecimal.ZERO : BigDecimal.valueOf(item.getFlowPrice()))
                .reduce(BigDecimal.ZERO, BigDecimal::add);
        progress.setCurrentSelfPurchaseAmount(selfPurchaseAmount);
        progress.setCurrentConsumeCount(paidOrders.size());
        // 各子条件为 AND 关系，全部满足才算达标
        boolean satisfied = true;
        if (requireSelfPurchaseAmount) {
            BigDecimal minAmount = setting.getMinSelfPurchaseAmount() == null
                    ? BigDecimal.ZERO : setting.getMinSelfPurchaseAmount();
            satisfied = selfPurchaseAmount.compareTo(minAmount) >= 0;
        }
        if (requireConsumeCount) {
            int minCount = setting.getMinConsumeCount() == null ? 0 : setting.getMinConsumeCount();
            satisfied = satisfied && paidOrders.size() >= minCount;
        }
        if (requirePurchaseGoods) {
            boolean purchaseGoodsSatisfied = hasPurchasedAnyGoods(paidOrders, setting.getRecruitGoods());
            progress.setPurchaseGoodsSatisfied(purchaseGoodsSatisfied);
            satisfied = satisfied && purchaseGoodsSatisfied;
        }
        progress.setSatisfied(satisfied);
        return progress;
    }

    @Override
    public DistributionRecruitApplication getPendingApplicationByMemberId(String memberId) {
        return applicationMapper.selectOne(new LambdaQueryWrapper<DistributionRecruitApplication>()
                .eq(DistributionRecruitApplication::getMemberId, memberId)
                .eq(DistributionRecruitApplication::getApplicationStatus, "PENDING")
                .orderByDesc(DistributionRecruitApplication::getCreateTime)
                .last("limit 1"));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void syncPendingApplyDistributions() {
        List<DistributionRecruitApplication> pendingApplications = applicationMapper.selectList(
                new LambdaQueryWrapper<DistributionRecruitApplication>()
                        .eq(DistributionRecruitApplication::getApplicationStatus, "PENDING")
                        .orderByDesc(DistributionRecruitApplication::getCreateTime));
        for (DistributionRecruitApplication application : pendingApplications) {
            Distribution distribution = null;
            if (application.getDistributionId() != null) {
                distribution = distributionMemberSupport.getById(application.getDistributionId());
            }
            // 分销员已是 APPLY 状态则无需重复补建
            if (distribution != null
                    && DistributionStatusEnum.APPLY.name().equals(distribution.getDistributionStatus())) {
                continue;
            }
            Member member = memberService.getById(application.getMemberId());
            distribution = distributionMemberSupport.markAsPendingApply(
                    application.getMemberId(), member == null ? "" : member.getNickName());
            if (!distribution.getId().equals(application.getDistributionId())) {
                application.setDistributionId(distribution.getId());
                applicationMapper.updateById(application);
            }
        }
    }

    /**
     * 保存前规范化招募设置（条件类型、指定商品等）。
     *
     * @param setting 招募设置
     */
    private void prepareSettingForSave(DistributionRecruitSetting setting) {
        // 非条件加入模式时清空所有门槛配置
        if (!RecruitJoinConditionTypeEnum.CONDITIONAL.name().equals(setting.getJoinConditionType())) {
            setting.setRequirePurchaseGoods(false);
            setting.setRequireSelfPurchaseAmount(false);
            setting.setRequireConsumeCount(false);
            setting.setRecruitGoods(Collections.emptyList());
        }
        if (!Boolean.TRUE.equals(setting.getRequirePurchaseGoods())) {
            setting.setRecruitGoods(Collections.emptyList());
        } else if (setting.getRecruitGoods() == null || setting.getRecruitGoods().isEmpty()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择指定商品");
        } else {
            List<DistributionRecruitPlanGoodsItemVO> normalized = new ArrayList<>();
            for (DistributionRecruitPlanGoodsItemVO item : setting.getRecruitGoods()) {
                if (item == null || item.getGoodsId() == null || item.getGoodsId().isBlank()) {
                    continue;
                }
                DistributionRecruitPlanGoodsItemVO copy = new DistributionRecruitPlanGoodsItemVO();
                copy.setGoodsId(item.getGoodsId());
                copy.setSkuId(item.getSkuId());
                normalized.add(copy);
            }
            setting.setRecruitGoods(normalized);
        }
    }

    /**
     * 为招募指定商品补充名称、规格、价格等展示信息。
     *
     * @param setting 招募设置
     */
    private void enrichRecruitGoods(DistributionRecruitSetting setting) {
        if (setting == null || setting.getRecruitGoods() == null || setting.getRecruitGoods().isEmpty()) {
            if (setting != null) {
                setting.setRecruitGoods(Collections.emptyList());
            }
            return;
        }
        List<DistributionRecruitPlanGoodsItemVO> enriched = new ArrayList<>();
        for (DistributionRecruitPlanGoodsItemVO item : setting.getRecruitGoods()) {
            if (item == null || item.getGoodsId() == null || item.getGoodsId().isBlank()) {
                continue;
            }
            DistributionRecruitPlanGoodsItemVO itemVO = new DistributionRecruitPlanGoodsItemVO();
            itemVO.setGoodsId(item.getGoodsId());
            itemVO.setSkuId(item.getSkuId());
            if (item.getSkuId() != null && !item.getSkuId().isBlank()) {
                GoodsSku goodsSku = goodsSkuService.getById(item.getSkuId());
                if (goodsSku != null) {
                    itemVO.setGoodsName(goodsSku.getGoodsName());
                    itemVO.setSn(goodsSku.getSn());
                    itemVO.setSimpleSpecs(goodsSku.getSimpleSpecs());
                    itemVO.setThumbnail(goodsSku.getThumbnail());
                    itemVO.setPrice(goodsSku.getPrice());
                }
            } else {
                Goods goods = goodsService.getById(item.getGoodsId());
                if (goods != null) {
                    itemVO.setGoodsName(goods.getGoodsName());
                    itemVO.setThumbnail(goods.getThumbnail());
                    itemVO.setPrice(goods.getPrice());
                }
            }
            enriched.add(itemVO);
        }
        setting.setRecruitGoods(enriched);
    }

    /**
     * 判断会员是否已购买招募指定商品中的任意一件。
     *
     * @param paidOrders   已支付订单列表
     * @param recruitGoods 招募指定商品
     * @return 是否已购买
     */
    private boolean hasPurchasedAnyGoods(List<Order> paidOrders,
                                         List<DistributionRecruitPlanGoodsItemVO> recruitGoods) {
        if (recruitGoods == null || recruitGoods.isEmpty()) {
            return false;
        }
        if (paidOrders == null || paidOrders.isEmpty()) {
            return false;
        }
        List<String> orderSns = paidOrders.stream()
                .map(Order::getSn)
                .filter(sn -> sn != null && !sn.isBlank())
                .collect(Collectors.toList());
        if (orderSns.isEmpty()) {
            return false;
        }
        List<OrderItem> orderItems = orderItemService.list(new LambdaQueryWrapper<OrderItem>()
                .in(OrderItem::getOrderSn, orderSns));
        for (OrderItem orderItem : orderItems) {
            for (DistributionRecruitPlanGoodsItemVO planGoods : recruitGoods) {
                // 指定了 SKU 则精确匹配 SKU，否则按商品 ID 匹配
                if (planGoods.getSkuId() != null && !planGoods.getSkuId().isBlank()) {
                    if (planGoods.getSkuId().equals(orderItem.getSkuId())) {
                        return true;
                    }
                    continue;
                }
                if (planGoods.getGoodsId() != null && planGoods.getGoodsId().equals(orderItem.getGoodsId())) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * 构建申请表单快照 JSON。
     *
     * @param setting   招募设置
     * @param formItems 用户填写的表单项
     * @return 快照 JSON，无表单项时返回 null
     */
    private String buildApplyFormSnapshot(DistributionRecruitSetting setting,
                                          List<DistributionRecruitApplyDTO.FormItem> formItems) {
        if (formItems == null || formItems.isEmpty()) {
            return null;
        }
        List<DistributionRecruitApplyFormItemSnapshot> snapshots = new ArrayList<>();
        for (DistributionRecruitApplyDTO.FormItem formItem : formItems) {
            if (formItem == null) {
                continue;
            }
            DistributionRecruitApplyFormItemSnapshot snapshot = new DistributionRecruitApplyFormItemSnapshot();
            snapshot.setFieldId(formItem.getFieldId());
            snapshot.setFieldValue(formItem.getFieldValue());
            DistributionRecruitFieldSetting fieldConfig =
                    recruitSettingSupport.findField(setting, formItem.getFieldId());
            if (fieldConfig != null) {
                snapshot.setFieldType(fieldConfig.getFieldType());
                snapshot.setFieldName(fieldConfig.getFieldName());
            }
            snapshots.add(snapshot);
        }
        return snapshots.isEmpty() ? null : JSONUtil.toJsonStr(snapshots);
    }
}
