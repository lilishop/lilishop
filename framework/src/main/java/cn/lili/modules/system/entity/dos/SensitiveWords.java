package cn.lili.modules.system.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;

/**
 * 敏感词实体
 * @author Bulbasaur
 * 2020-02-25 14:10:16
 */
@Data
@TableName("li_sensitive_words")
@ApiModel(value = "敏感词")
public class SensitiveWords extends BaseEntity {

    /**
     * 敏感词名称
     */
    @ApiModelProperty(value = "敏感词名称")
    @NotEmpty(message = "敏感词必填")
    @Size(min = 2, max = 20)
    private String sensitiveWord;

}