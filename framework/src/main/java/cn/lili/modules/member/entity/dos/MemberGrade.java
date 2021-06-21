package cn.lili.modules.member.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 * 会员等级
 *
 * @author Bulbasaur
 * @date 2021/5/14 5:43 下午
 */
@Data
@Entity
@Table(name = "li_member_grade")
@TableName("li_member_grade")
@ApiModel(value = "会员等级")
public class MemberGrade extends BaseEntity {

    @NotNull
    @ApiModelProperty(value = "等级名称")
    private String gradeName;

    @NotNull
    @ApiModelProperty(value = "等级图片")
    private String gradeImage;

    @NotNull
    @ApiModelProperty(value = "所需经验值")
    private Integer experienceValue;

    @ApiModelProperty(value = "是否为默认等级")
    private Boolean isDefault;
}
