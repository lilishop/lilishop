package cn.lili.modules.distribution.service;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 分销等级配置服务。
 * <p>
 * 管理分销等级的新增、编辑、升级规则校验及分销员等级刷新。
 * 分销员表中存储的是等级值（1-5），而非等级主键 ID。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionGradeService extends IService<DistributionGrade> {

    /**
     * 批量保存分销等级（整页保存，统一校验后写入）。
     *
     * @param grades 等级列表
     */
    void saveAllGrades(List<DistributionGrade> grades);

    /**
     * 删除分销等级（等级 1 不可删除）。
     *
     * @param id 等级 ID
     */
    void deleteGrade(String id);

    /**
     * 校验各等级佣金比例：全站最高商品佣金 + 最高邀请奖励不得超过 100%。
     *
     * @param grades 待校验等级列表
     */
    void validateCrossRates(List<DistributionGrade> grades);

    /**
     * 获取基础等级（等级值 1，普通分销员）。
     *
     * @return 基础等级配置
     */
    DistributionGrade getBaseGrade();

    /**
     * 根据等级值或历史主键 ID 解析等级配置。
     *
     * @param gradeRef 等级值（1-5）或等级主键 ID
     * @return 等级配置，不存在时返回 null
     */
    DistributionGrade resolveGrade(String gradeRef);

    /**
     * 解析等级配置；无法解析时按基础等级、买家可见等级、零比例占位依次回退，保证非 null。
     *
     * @param gradeRef 等级值（1-5）或等级主键 ID
     * @return 等级配置（永不为 null）
     */
    DistributionGrade resolveGradeWithFallback(String gradeRef);

    /**
     * 将分销员上的等级引用统一为等级值字符串。
     *
     * @param distribution 分销员
     */
    void normalizeGradeReference(Distribution distribution);

    /**
     * 将等级值格式化为字符串。
     *
     * @param sortOrder 等级值
     * @return 等级值字符串
     */
    String formatGradeValue(Integer sortOrder);

    /**
     * 为分销员分配默认等级值 1（若尚未设置等级）。
     *
     * @param distribution 分销员
     */
    void assignBaseGradeIfAbsent(Distribution distribution);

    /**
     * 根据升级规则刷新分销员等级，满足条件时自动升级并记录历史。
     *
     * @param distributionId 分销员 ID
     */
    void refreshGrade(String distributionId);

    /**
     * 查询全部等级列表（按等级值升序）。
     *
     * @return 等级列表
     */
    List<DistributionGrade> listGrades();

    /**
     * 买家端可见等级列表（排除已停用等级）。
     *
     * @return 等级列表
     */
    List<DistributionGrade> listBuyerGrades();
}
