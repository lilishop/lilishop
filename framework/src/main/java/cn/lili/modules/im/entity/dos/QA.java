package cn.lili.modules.im.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 问题答案
 *
 * @author Chopper
 * @version v1.0
 * 2022-02-09 17:59
 */
@Data
@TableName("li_qa")
@ApiModel(value = "租户问答")
@NoArgsConstructor
public class QA extends BaseEntity {

    @ApiModelProperty(value = "租户id")
    private Integer tenantId;

    @ApiModelProperty(value = "问题")
    private String question;

    @ApiModelProperty(value = "答案")
    private String answer;
}
