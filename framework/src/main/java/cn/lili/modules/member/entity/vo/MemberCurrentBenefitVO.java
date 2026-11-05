package cn.lili.modules.member.entity.vo;

import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberGrade;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.List;

/**
 * 当前客户权益信息
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "当前客户权益信息")
public class MemberCurrentBenefitVO implements Serializable {

    @Schema(description = "当前等级ID")
    private String gradeId;

    @Schema(description = "当前等级信息")
    private MemberGrade grade;

    @Schema(description = "当前等级绑定的权益列表")
    private List<MemberBenefit> benefits;

    /** 当前客户累计经验值，最小为0，用于升级进度展示。 */
    @Schema(description = "当前累计经验值，最小为0")
    private Long experience;

    /** 下一启用客户等级，对应 li_member_grade；最高等级时为空。 */
    @Schema(description = "下一启用等级，按经验门槛选择；最高等级时为空")
    private MemberGrade nextGrade;

    /** 达到下一启用等级还差的经验值，最高等级时为0。 */
    @Schema(description = "距下一等级所差经验值，最高等级时为0")
    private Long remainingExperience;

    /** 当前等级到下一等级的升级进度，单位百分比，取值0～100；最高等级为100。 */
    @Schema(description = "当前等级至下一等级的升级进度，整数0～100；最高等级为100")
    private Integer upgradeProgress;

    /** 是否已处于最高启用等级；未配置当前等级时为false。 */
    @Schema(description = "是否已处于最高启用等级；未配置当前等级时为false")
    private Boolean highestGrade;
}
