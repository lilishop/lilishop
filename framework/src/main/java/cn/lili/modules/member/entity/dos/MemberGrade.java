package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 客户等级
 *
 * @author Bulbasaur
 * @since 2021/5/14 5:43 下午
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_member_grade")
@Schema(description = "客户等级")
public class MemberGrade extends BaseEntity {

    @NotBlank
    @Size(min = 1, max = 50)
    @Schema(description = "等级名称")
    private String gradeName;

    @Schema(description = "是否默认等级")
    private Boolean isDefault;

    @NotBlank
    @Schema(description = "等级图标URL")
    private String gradeImage;

    @Schema(description = "等级背景图URL")
    private String gradeBackground;

    @Schema(description = "等级字体颜色")
    private String gradeFontColor;

    @NotNull
    @Min(1)
    @Schema(description = "所需经验值")
    private Integer requiredExperience;

    @NotNull
    @Min(1)
    @Max(9999)
    @Schema(description = "等级排序")
    private Integer gradeSort;

    @NotBlank
    @Schema(description = "等级开关 OPEN/CLOSE")
    private String gradeState;

    @Schema(description = "会员权益ID，多个用英文逗号分隔")
    private String benefitIds;

}
