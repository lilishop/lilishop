package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionGradeHistory;
import cn.lili.modules.distribution.entity.dto.DistributionGradeHistorySearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGradeHistoryVO;
import cn.lili.modules.distribution.mapper.DistributionGradeHistoryMapper;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionGradeHistoryService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 分销等级变更历史服务实现
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@Service
public class DistributionGradeHistoryServiceImpl extends ServiceImpl<DistributionGradeHistoryMapper, DistributionGradeHistory>
        implements DistributionGradeHistoryService {

    @Autowired
    private DistributionMapper distributionMapper;

    @Autowired
    private MemberService memberService;

    @Autowired
    private DistributionGradeService distributionGradeService;

    @Override
    public IPage<DistributionGradeHistoryVO> historyPage(DistributionGradeHistorySearchParams params) {
        DistributionGradeHistorySearchParams safeParams =
                params == null ? new DistributionGradeHistorySearchParams() : params;
        LambdaQueryWrapper<DistributionGradeHistory> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.orderByDesc(DistributionGradeHistory::getUpgradeTime);

        // 按分销员 ID、升级后等级值筛选
        if (StringUtils.isNotEmpty(safeParams.getDistributionId())) {
            queryWrapper.eq(DistributionGradeHistory::getDistributionId, safeParams.getDistributionId());
        }
        if (StringUtils.isNotEmpty(safeParams.getGradeId())) {
            queryWrapper.eq(DistributionGradeHistory::getGradeId, safeParams.getGradeId().trim());
        }

        // 按分销员昵称/手机号反查分销员 ID 列表
        List<String> distributionIds = resolveDistributionIds(safeParams);
        if (distributionIds != null) {
            if (distributionIds.isEmpty()) {
                return emptyPage(safeParams);
            }
            queryWrapper.in(DistributionGradeHistory::getDistributionId, distributionIds);
        }

        // 升级时间范围
        Date upgradeTimeStart = safeParams.getUpgradeTimeStart();
        Date upgradeTimeEnd = safeParams.getUpgradeTimeEnd();
        if (upgradeTimeStart != null && upgradeTimeEnd != null) {
            queryWrapper.between(DistributionGradeHistory::getUpgradeTime, upgradeTimeStart,
                    DateUtil.endOfDate(upgradeTimeEnd));
        } else if (upgradeTimeStart != null) {
            queryWrapper.ge(DistributionGradeHistory::getUpgradeTime, upgradeTimeStart);
        } else if (upgradeTimeEnd != null) {
            queryWrapper.le(DistributionGradeHistory::getUpgradeTime, DateUtil.endOfDate(upgradeTimeEnd));
        }

        IPage<DistributionGradeHistory> historyPage =
                this.page(PageUtil.initPage(safeParams), queryWrapper);
        Page<DistributionGradeHistoryVO> result = new Page<>(
                historyPage.getCurrent(), historyPage.getSize(), historyPage.getTotal());
        result.setRecords(historyPage.getRecords().stream().map(this::toVO).toList());
        return result;
    }

    /**
     * 按分销员筛选条件解析分销员 ID 列表；无相关筛选时返回 null。
     */
    private List<String> resolveDistributionIds(DistributionGradeHistorySearchParams params) {
        boolean hasDistributionFilter = StringUtils.isNotEmpty(params.getMemberName())
                || StringUtils.isNotEmpty(params.getDistributionMobile());
        if (!hasDistributionFilter) {
            return null;
        }
        LambdaQueryWrapper<Distribution> distributionWrapper = new LambdaQueryWrapper<>();
        if (StringUtils.isNotEmpty(params.getMemberName())) {
            distributionWrapper.like(Distribution::getMemberName, params.getMemberName().trim());
        }
        // 手机号先查会员，再关联分销员
        if (StringUtils.isNotEmpty(params.getDistributionMobile())) {
            List<String> memberIds = memberService.list(new LambdaQueryWrapper<Member>()
                            .like(Member::getMobile, params.getDistributionMobile().trim()))
                    .stream()
                    .map(Member::getId)
                    .collect(Collectors.toList());
            if (memberIds.isEmpty()) {
                return Collections.emptyList();
            }
            distributionWrapper.in(Distribution::getMemberId, memberIds);
        }
        return distributionMapper.selectList(distributionWrapper.select(Distribution::getId))
                .stream()
                .map(Distribution::getId)
                .collect(Collectors.toList());
    }

    /**
     * 构建空分页结果。
     *
     * @param page 分页参数
     * @return 空分页对象
     */
    private Page<DistributionGradeHistoryVO> emptyPage(PageVO page) {
        Page<DistributionGradeHistoryVO> result = new Page<>(page.getPageNumber(), page.getPageSize(), 0);
        result.setRecords(Collections.emptyList());
        return result;
    }

    /**
     * 将等级变更历史转换为 VO。
     *
     * @param history 等级变更历史
     * @return 展示 VO
     */
    private DistributionGradeHistoryVO toVO(DistributionGradeHistory history) {
        DistributionGradeHistoryVO vo = new DistributionGradeHistoryVO();
        vo.setId(history.getId());
        vo.setDistributionId(history.getDistributionId());
        vo.setPreviousGradeId(history.getPreviousGradeId());
        vo.setGradeId(history.getGradeId());
        vo.setGradeName(history.getGradeName());
        vo.setGoodsCommissionRate(history.getGoodsCommissionRate());
        vo.setInviteCommissionRate(history.getInviteCommissionRate());
        vo.setUpgradeTime(history.getUpgradeTime());

        // 补全分销员昵称与手机号
        if (StringUtils.isNotEmpty(history.getDistributionId())) {
            Distribution distribution = distributionMapper.selectById(history.getDistributionId());
            if (distribution != null) {
                vo.setMemberName(distribution.getMemberName());
                if (StringUtils.isNotEmpty(distribution.getMemberId())) {
                    Member member = memberService.getById(distribution.getMemberId());
                    if (member != null) {
                        vo.setMemberMobile(member.getMobile());
                    }
                }
            }
        }
        // 变更前等级名；快照未存等级名时按等级值回查
        vo.setPreviousGradeName(resolveGradeName(history.getPreviousGradeId()));
        if (CharSequenceUtil.isBlank(vo.getGradeName())) {
            vo.setGradeName(resolveGradeName(history.getGradeId()));
        }
        return vo;
    }

    /**
     * 根据等级引用解析等级名称。
     *
     * @param gradeRef 等级 ID 或排序值
     * @return 等级名称
     */
    private String resolveGradeName(String gradeRef) {
        if (CharSequenceUtil.isBlank(gradeRef)) {
            return null;
        }
        DistributionGrade grade = distributionGradeService.resolveGrade(gradeRef);
        if (grade != null && CharSequenceUtil.isNotBlank(grade.getGradeName())) {
            return grade.getGradeName();
        }
        return "等级值" + gradeRef;
    }
}
