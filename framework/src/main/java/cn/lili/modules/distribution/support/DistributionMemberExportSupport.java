package cn.lili.modules.distribution.support;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.enums.DistributionRewardTypeEnum;
import cn.lili.modules.distribution.mapper.DistributionCustomerRelationMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * 分销员导出业务支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class DistributionMemberExportSupport {

    @Autowired
    private MemberService memberService;

    @Autowired
    private ConnectService connectService;

    @Autowired
    private DistributionGradeService gradeService;

    @Autowired
    private DistributionCustomerRelationMapper customerRelationMapper;

    public DistributionMemberExportSupport() {
    }

    public DistributionMemberExportSupport(MemberService memberService,
                                         ConnectService connectService,
                                         DistributionGradeService gradeService,
                                         DistributionCustomerRelationMapper customerRelationMapper) {
        this.memberService = memberService;
        this.connectService = connectService;
        this.gradeService = gradeService;
        this.customerRelationMapper = customerRelationMapper;
    }

    /**
     * 导出分销员列表至 Excel
     *
     * @param distributionService 分销服务实例
     * @param distributionMapper 分销 Mapper 实例
     * @param response           HTTP 响应
     * @param distributions      分销员记录列表
     */
    public void exportMembers(DistributionService distributionService, DistributionMapper distributionMapper,
                              HttpServletResponse response, List<Distribution> distributions) {
        FinanceExportHelper.checkRowLimit(distributions.size());

        // 批量预加载会员、openid 及邀请人信息，避免逐行查库
        Set<String> memberIds = distributions.stream()
                .map(Distribution::getMemberId)
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toSet());
        Map<String, Member> memberMap = memberIds.isEmpty() ? Collections.emptyMap()
                : memberService.listByIds(memberIds).stream()
                .collect(Collectors.toMap(Member::getId, item -> item, (a, b) -> a));
        Map<String, String> openIdMap = loadOpenIdMap(memberIds);

        Set<String> inviterDistributionIds = distributions.stream()
                .map(Distribution::getParentDistributionId)
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toSet());
        Map<String, Distribution> inviterMap = inviterDistributionIds.isEmpty() ? Collections.emptyMap()
                : distributionService.listByIds(inviterDistributionIds).stream()
                .collect(Collectors.toMap(Distribution::getId, item -> item, (a, b) -> a));
        Set<String> inviterMemberIds = inviterMap.values().stream()
                .map(Distribution::getMemberId)
                .filter(StringUtils::isNotEmpty)
                .collect(Collectors.toSet());
        Map<String, Member> inviterMemberMap = inviterMemberIds.isEmpty() ? Collections.emptyMap()
                : memberService.listByIds(inviterMemberIds).stream()
                .collect(Collectors.toMap(Member::getId, item -> item, (a, b) -> a));
        Map<String, String> inviterOpenIdMap = loadOpenIdMap(inviterMemberIds);

        List<String> headers = List.of(
                "手机号", "分销员openid", "昵称", "等级", "邀请方", "邀请方分销员openid", "加入时间",
                "累计客户", "有效客户", "累计邀请", "累计收益", "累计成交金额", "累计首单金额",
                "累计推广订单数", "累计推广金额", "商品佣金", "邀请奖励");
        List<List<Object>> rows = new ArrayList<>();
        for (Distribution distribution : distributions) {
            Member member = memberMap.get(distribution.getMemberId());
            String mobile = member == null ? "" : Objects.toString(member.getMobile(), "");
            String nickname = member == null ? Objects.toString(distribution.getMemberName(), "")
                    : Objects.toString(member.getNickName(), "");
            String openId = openIdMap.getOrDefault(distribution.getMemberId(), "");
            String gradeName = "";
            if (distribution.getGradeId() != null) {
                DistributionGrade grade = gradeService.resolveGrade(distribution.getGradeId());
                if (grade != null) {
                    gradeName = Objects.toString(grade.getGradeName(), "");
                }
            }
            String inviterName = "";
            String inviterOpenId = "";
            if (StringUtils.isNotEmpty(distribution.getParentDistributionId())) {
                Distribution inviter = inviterMap.get(distribution.getParentDistributionId());
                if (inviter != null) {
                    Member inviterMember = inviterMemberMap.get(inviter.getMemberId());
                    inviterName = inviterMember == null
                            ? Objects.toString(inviter.getMemberName(), "")
                            : Objects.toString(inviterMember.getNickName(), inviter.getMemberName());
                    inviterOpenId = inviterOpenIdMap.getOrDefault(inviter.getMemberId(), "");
                }
            }
            Long totalCustomers = customerRelationMapper.selectCount(new LambdaQueryWrapper<DistributionCustomerRelation>()
                    .eq(DistributionCustomerRelation::getDistributionId, distribution.getId()));
            BigDecimal goodsCommission = defaultAmount(distributionMapper.sumRewardByType(
                    distribution.getId(), DistributionRewardTypeEnum.DIRECT.name()));
            BigDecimal inviteCommission = defaultAmount(distributionMapper.sumRewardByType(
                    distribution.getId(), DistributionRewardTypeEnum.INVITE.name()));
            BigDecimal promotionAmount = defaultAmount(distributionMapper.sumOrderSales(distribution.getId(), 0));
            BigDecimal firstOrderAmount = defaultAmount(distributionMapper.sumFirstOrderAmount(distribution.getId()));
            Long promotionOrderCount = distributionMapper.countPromotionOrders(distribution.getId());

            rows.add(List.of(
                    mobile,
                    openId,
                    nickname,
                    gradeName,
                    inviterName == null ? "" : inviterName,
                    inviterOpenId,
                    distribution.getJoinTime() == null ? ""
                            : cn.hutool.core.date.DateUtil.formatDateTime(distribution.getJoinTime()),
                    totalCustomers == null ? 0 : totalCustomers,
                    distribution.getValidCustomerCount() == null ? 0 : distribution.getValidCustomerCount(),
                    distribution.getInviteCount() == null ? 0 : distribution.getInviteCount(),
                    defaultAmount(distribution.getValidCommissionAmount()),
                    defaultAmount(distribution.getValidSalesAmount()),
                    firstOrderAmount,
                    promotionOrderCount == null ? 0 : promotionOrderCount,
                    promotionAmount,
                    goodsCommission,
                    inviteCommission
            ));
        }

        FinanceExportHelper.writeExcel(response, "分销员列表", writer ->
                FinanceExportHelper.writeSheet(writer, "分销员列表", headers, rows));
    }

    /**
     * 批量加载会员对应的微信小程序 openid
     *
     * @param memberIds 会员ID集合
     * @return 会员ID与 openid 的映射
     */
    public Map<String, String> loadOpenIdMap(Set<String> memberIds) {
        if (memberIds == null || memberIds.isEmpty()) {
            return Collections.emptyMap();
        }
        Map<String, String> openIdMap = new HashMap<>();
        connectService.list(new LambdaQueryWrapper<Connect>()
                        .in(Connect::getUserId, memberIds)
                        .eq(Connect::getUnionType, SourceEnum.WECHAT_MP_OPEN_ID.name()))
                .forEach(connect -> {
                    if (StringUtils.isNotEmpty(connect.getUserId()) && StringUtils.isNotEmpty(connect.getUnionId())) {
                        openIdMap.putIfAbsent(connect.getUserId(), connect.getUnionId());
                    }
                });
        return openIdMap;
    }

    private BigDecimal defaultAmount(BigDecimal amount) {
        return amount == null ? BigDecimal.ZERO : amount;
    }
}
