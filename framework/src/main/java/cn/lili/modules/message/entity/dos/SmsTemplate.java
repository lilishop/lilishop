package cn.lili.modules.message.entity.dos;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;


/**
 * 短信模板
 *
 * @author Chopper
 * @date 2021/1/30 4:13 下午
 */
@Data
@Entity
@Table(name = "li_sms_template")
@TableName("li_sms_template")
@ApiModel(value = "短信模板")
public class SmsTemplate {

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "模板名称", required = true)
    private String templateName;

    @ApiModelProperty(value = "短信类型", required = true)
    private Integer templateType;

    @ApiModelProperty(value = "短信模板申请说明", required = true)
    private String remark;

    @ApiModelProperty(value = "模板内容", required = true)
    private String templateContent;

    /**
     * 0：审核中。
     * 1：审核通过。
     * 2：审核失败，请在返回参数Reason中查看审核失败原因。
     */
    @ApiModelProperty(value = "模板审核状态")
    private Integer templateStatus;

    @ApiModelProperty(value = "短信模板CODE")
    private String templateCode;

    @ApiModelProperty(value = "审核备注")
    private String reason;

}
