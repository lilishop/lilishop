package cn.lili.modules.distribution.support;

import cn.hutool.core.util.RandomUtil;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * 分销员实体读写辅助，仅依赖 Mapper 与同层 ReferenceSupport，不注入 Service。
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionMemberSupport {

    @Autowired
    private DistributionMapper distributionMapper;

    @Autowired
    private DistributionGradeReferenceSupport distributionGradeReferenceSupport;

    /**
     * 按主键查询分销员。
     *
     * @param id 分销员ID
     * @return 分销员实体
     */
    public Distribution getById(String id) {
        return distributionMapper.selectById(id);
    }

    /**
     * 按会员ID查询分销员。
     *
     * @param memberId 会员ID
     * @return 分销员实体
     */
    public Distribution getByMemberId(String memberId) {
        return distributionMapper.selectOne(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getMemberId, memberId));
    }

    /**
     * 新增分销员记录。
     *
     * @param distribution 分销员实体
     */
    public void save(Distribution distribution) {
        distributionMapper.insert(distribution);
    }

    /**
     * 按主键更新分销员记录。
     *
     * @param distribution 分销员实体
     */
    public void updateById(Distribution distribution) {
        distributionMapper.updateById(distribution);
    }

    /**
     * 判断当前分销员状态是否阻止再次提交招募申请。
     *
     * @param distribution 分销员实体
     * @return true 表示不可再次申请
     */
    public boolean isBlockingRecruitApply(Distribution distribution) {
        // 仅「已拒绝」与无记录可申请；清退为停用，不可再申请
        return distribution != null
                && !DistributionStatusEnum.REFUSE.name().equals(distribution.getDistributionStatus());
    }

    /**
     * 生成全局唯一的邀请码。
     *
     * @return 8位大写邀请码
     */
    public String generateInviteCode() {
        String code;
        // 碰撞则重试，直至全局唯一
        do {
            code = RandomUtil.randomStringUpper(8);
        } while (distributionMapper.selectCount(new LambdaQueryWrapper<Distribution>()
                .eq(Distribution::getInviteCode, code)) > 0);
        return code;
    }

    /**
     * 招募人工审核待审时，创建或更新分销员为申请中状态。
     *
     * @param memberId   会员ID
     * @param memberName 会员昵称
     * @return 分销员实体
     */
    public Distribution markAsPendingApply(String memberId, String memberName) {
        Distribution distribution = getByMemberId(memberId);
        if (distribution == null) {
            // 首次申请：新建记录并置为申请中
            distribution = new Distribution();
            distribution.setMemberId(memberId);
            distribution.setMemberName(memberName == null ? "" : memberName);
            distribution.setDistributionStatus(DistributionStatusEnum.APPLY.name());
            save(distribution);
            return distribution;
        }
        // 再次申请：刷新昵称并回退为申请中
        if (memberName != null && !memberName.isBlank()) {
            distribution.setMemberName(memberName);
        }
        distribution.setDistributionStatus(DistributionStatusEnum.APPLY.name());
        updateById(distribution);
        return distribution;
    }

    /**
     * 招募审核通过后创建或激活分销员。
     *
     * @param memberId   会员ID
     * @param memberName 会员昵称
     * @return 分销员实体
     */
    public Distribution activateOnRecruitPass(String memberId, String memberName) {
        Distribution distribution = getByMemberId(memberId);
        if (distribution == null) {
            // 新分销员：写入通过状态、加入时间与邀请码
            distribution = new Distribution();
            distribution.setMemberId(memberId);
            distribution.setMemberName(memberName == null ? "" : memberName);
            distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
            distribution.setJoinTime(new Date());
            distribution.setInviteCode(generateInviteCode());
            distributionGradeReferenceSupport.assignBaseGradeIfAbsent(distribution);
            save(distribution);
            return distribution;
        }
        // 历史记录重新通过：保留原邀请码，仅补全缺失项
        distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
        distribution.setJoinTime(new Date());
        if (distribution.getInviteCode() == null) {
            distribution.setInviteCode(generateInviteCode());
        }
        distributionGradeReferenceSupport.assignBaseGradeIfAbsent(distribution);
        updateById(distribution);
        return distribution;
    }
}
