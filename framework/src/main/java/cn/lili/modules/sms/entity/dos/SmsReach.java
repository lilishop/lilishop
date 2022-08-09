package cn.lili.modules.sms.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;


/**
 * 短信任务
 *
 * @author Chopper
 * @since 2021/1/30 4:13 下午
 */
@Data
@TableName("li_sms_reach")
@ApiModel(value = "短信任务")
@EqualsAndHashCode(callSuper = true)
public class SmsReach extends BaseIdEntity {

    private static final long serialVersionUID = -8106666482841131277L;

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
