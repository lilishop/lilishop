package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * 商品参数
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_parameters")
@ApiModel(value = "商品参数")
public class Parameters extends BaseIdEntity {


    private static final long serialVersionUID = -566510714456317006L;

    @ApiModelProperty(value = "参数名称", required = true)
    @NotEmpty(message = "参数名称必填")
    @Length(max = 5, message = "参数名称不能超过5字")
    private String paramName;


    @ApiModelProperty(value = "选择值")
    @NotEmpty(message = "参数选项值必填")
    @Length(max = 255, message = "参数选项过长，请简略")
    private String options;

    @ApiModelProperty(value = "是否可索引，0 不显示 1 显示", required = true)
    @NotNull(message = "是否可索引必选")
    @Min(value = 0, message = "是否可索引传值不正确")
    @Max(value = 1, message = "是否可索引传值不正确")
    private Integer isIndex;

    @ApiModelProperty(value = "是否必填是  1    否   0", required = true)
    @NotNull(message = "是否必填必选")
    @Min(value = 0, message = "是否必填传值不正确")
    @Max(value = 1, message = "是否必填传值不正确")
    private Integer required;

    @ApiModelProperty(value = "参数分组id", required = true)
    @NotNull(message = "所属参数组不能为空")
    private String groupId;

    @ApiModelProperty(value = "分类id", hidden = true)
    private String categoryId;

    @ApiModelProperty(value = "排序", hidden = true)
    @NotNull(message = "请输入排序值")
    @Max(value = 9999, message = "排序值不能大于9999")
    private Integer sort;

}