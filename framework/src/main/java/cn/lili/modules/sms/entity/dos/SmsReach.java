package cn.lili.modules.sms.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
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
@Schema(description = "短信任务")
@EqualsAndHashCode(callSuper = true)
public class SmsReach extends BaseIdEntity {

    private static final long serialVersionUID = -8106666482841131277L;

    @Schema(description = "签名名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String signName;

    @Schema(description = "模板名称")
    private String smsName;

    @Schema(description = "消息CODE")
    private String messageCode;

    @Schema(description = "消息内容")
    private String context;

    @Schema(description = "接收人", allowableValues = "1:全部客户，2：选择客户")
    private String smsRange;

    @Schema(description = "预计发送条数")
    private String num;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;
}
