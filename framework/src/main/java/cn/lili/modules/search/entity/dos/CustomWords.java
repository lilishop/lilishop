package cn.lili.modules.search.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.NotEmpty;

/**
 * 自定义分词
 *
 * @author paulG
 * @since 2020/10/15
 **/
@Data
@TableName("li_custom_words")
@ApiModel(value = "自定义分词")
@EqualsAndHashCode(callSuper = true)
public class CustomWords extends BaseEntity {

    private static final long serialVersionUID = 650889506808657977L;

    /**
     * 名称
     */
    @ApiModelProperty(value = "名称")
    @NotEmpty(message = "分词名称必填")
    @Length(max = 20, message = "分词名称长度不能大于20")
    private String name;


    @ApiModelProperty(value = "是否禁用")
    private Integer disabled;


}
