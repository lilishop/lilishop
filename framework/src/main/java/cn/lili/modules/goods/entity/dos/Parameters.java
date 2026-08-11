package cn.lili.modules.goods.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

/**
 * 商品参数
 *
 * @author pikachu
 * @since 2020-02-23 9:14:33
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_parameters")
@Schema(description = "商品参数")
public class Parameters extends BaseEntity {


    private static final long serialVersionUID = -566510714456317006L;

    @Schema(description = "参数名称", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotEmpty(message = "参数名称必填")
    @Length(max = 5, message = "参数名称不能超过5字")
    private String paramName;


    @Schema(description = "选择值")
    @NotEmpty(message = "参数选项值必填")
    @Length(max = 255, message = "参数选项过长，请简略")
    private String options;

    @Schema(description = "是否可索引，0 不显示 1 显示", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotNull(message = "是否可索引必选")
    @Min(value = 0, message = "是否可索引传值不正确")
    @Max(value = 1, message = "是否可索引传值不正确")
    private Integer isIndex;

    @Schema(description = "是否必填是  1    否   0", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotNull(message = "是否必填必选")
    @Min(value = 0, message = "是否必填传值不正确")
    @Max(value = 1, message = "是否必填传值不正确")
    private Integer required;

    @Schema(description = "排序", hidden = true)
    @NotNull(message = "请输入排序值")
    @Max(value = 9999, message = "排序值不能大于9999")
    private Integer sort;

}
