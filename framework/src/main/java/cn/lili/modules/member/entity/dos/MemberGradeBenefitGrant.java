package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 客户等级权益首次初始化记录，对应 li_member_grade_benefit_grant 表。
 *
 * <p>同一客户、同一等级仅记录一次，表示系统已经处理过该等级的首次权益初始化，
 * 并非该等级所有权益均已发放成功。具体权益的状态、次数和失败原因由
 * {@link MemberBenefitDelivery} 分别记录。</p>
 *
 * <p>继承的 createTime 是该等级权益首次初始化时间，用作周期权益的计算起点。
 * 已有本记录但缺少一次性权益明细时，不新增一次性发放任务，以兼容旧数据并避免重复赠送；
 * 周期性权益仍可按各自周期创建发放明细。</p>
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_member_grade_benefit_grant")
@Schema(description = "等级权益首次初始化记录，不代表全部权益发放成功")
public class MemberGradeBenefitGrant extends BaseEntity {

    /** 首次初始化等级权益的客户 ID，对应 li_member.id；与 gradeId 构成唯一业务键。 */
    @Schema(description = "客户ID")
    private String memberId;

    /** 初始化权益时的客户等级 ID，对应 li_member_grade.id，不是单项权益 ID。 */
    @Schema(description = "客户等级ID")
    private String gradeId;
}
