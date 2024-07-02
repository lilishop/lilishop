package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * 会员等级
 *
 * @author Bulbasaur
 * @since 2021/5/14 5:43 下午
 */
@Data
@TableName("li_member_grade")
@ApiModel(value = "会员等级")
public class MemberGrade extends BaseEntity {

    @NotNull
    @ApiModelProperty(value = "等级名称")
    private String gradeName;

    @NotNull
    @ApiModelProperty(value = "等级图片 1029*498")
    private String gradeImage;

    @NotNull
    @ApiModelProperty(value = "会员等级")
    private Integer level;

    @ApiModelProperty(value = "累计实付")
    private Double payPrice;

    @ApiModelProperty(value = "累计购买次数")
    private Integer count;
}
