package cn.lili.modules.message.entity.dos;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;


/**
 * 短信任务
 *
 * @author Chopper
 * @date 2021/1/30 4:13 下午
 */
@Data
@Entity
@Table(name = "li_sms_reach")
@TableName("li_sms_reach")
@ApiModel(value = "短信任务")
public class SmsReach {

    @Id
    @TableId
    @TableField
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "签名名称", required = true)
    private String signName;

    @ApiModelProperty(value = "模板名称")
    private String smsName;

    @ApiModelProperty(value = "消息CODE")
    private String messageCode;

    @ApiModelProperty(value = "消息内容")
    private String context;

    @ApiModelProperty(value = "接收人", allowableValues = "1:全部会员，2：选择会员")
    private String smsRange;

    @ApiModelProperty(value = "预计发送条数")
    private String num;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;
}
