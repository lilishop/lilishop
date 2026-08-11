package cn.lili.modules.sms.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;


/**
 * 短信模板
 *
 * @author Chopper
 * @since 2021/1/30 4:13 下午
 */
@Data
@TableName("li_sms_template")
@Schema(description = "短信模板")
@EqualsAndHashCode(callSuper = false)
public class SmsTemplate extends BaseIdEntity {

    @Schema(description = "模板名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String templateName;

    @Schema(description = "短信类型", requiredMode = Schema.RequiredMode.REQUIRED)
    private Integer templateType;

    @Schema(description = "短信模板申请说明", requiredMode = Schema.RequiredMode.REQUIRED)
    private String remark;

    @Schema(description = "模板内容", requiredMode = Schema.RequiredMode.REQUIRED)
    private String templateContent;

    /**
     * 0：审核中。
     * 1：审核通过。
     * 2：审核失败，请在返回参数Reason中查看审核失败原因。
     */
    @Schema(description = "模板审核状态")
    private Integer templateStatus;

    @Schema(description = "短信模板CODE")
    private String templateCode;

    @Schema(description = "审核备注")
    private String reason;

}
