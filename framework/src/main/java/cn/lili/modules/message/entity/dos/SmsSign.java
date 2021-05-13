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
 * 短信签名
 *
 * @author Chopper
 * @date 2021/1/30 4:13 下午
 */
@Data
@Entity
@Table(name = "li_sms_sign")
@TableName("li_sms_sign")
@ApiModel(value = "短信签名")
public class SmsSign {

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "签名名称", required = true)
    private String signName;

    @ApiModelProperty(value = "签名来源", required = true)
    private Integer signSource;

    @ApiModelProperty(value = "短信签名申请说明", required = true)
    private String remark;

    @ApiModelProperty(value = "营业执照", required = true)
    private String businessLicense;

    @ApiModelProperty(value = "授权委托书", required = true)
    private String license;

    /**
     * 0：审核中。
     * 1：审核通过。
     * 2：审核失败，请在返回参数Reason中查看审核失败原因。
     */
    @ApiModelProperty(value = "签名审核状态")
    private Integer signStatus;

    @ApiModelProperty(value = "审核备注")
    private String reason;

}
