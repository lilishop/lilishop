package cn.lili.modules.member.entity.vo;

import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberGrade;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.List;

/**
 * 客户等级详情（含关联权益列表，管理端/买家端共用）
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@Schema(description = "客户等级详情")
public class MemberGradeDetailVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "等级基础信息")
    private MemberGrade grade;

    @Schema(description = "关联的客户权益列表（顺序与等级 benefitIds 配置一致；买家端列表仅含启用权益）")
    private List<MemberBenefit> benefits;
}
