package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 客户经验值流水
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_member_experience_log")
@Schema(description = "客户经验值流水")
public class MemberExperienceLog extends BaseEntity {

    @Schema(description = "客户ID")
    private String memberId;

    @Schema(description = "客户名称")
    private String memberName;

    @TableField(exist = false)
    @Schema(description = "客户手机号")
    private String memberMobile;

    @Schema(description = "规则编码")
    private String ruleKey;

    @Schema(description = "业务ID")
    private String bizId;

    @Schema(description = "当前经验值")
    private Long experience;

    @Schema(description = "变动前经验值")
    private Long beforeExperience;

    @Schema(description = "变动经验值")
    private Long variableExperience;

    @Schema(description = "备注")
    private String content;
}
