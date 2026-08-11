package cn.lili.modules.sms.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;


/**
 * 短信签名
 *
 * @author Chopper
 * @since 2021/1/30 4:13 下午
 */
@Data
@TableName("li_sms_sign")
@Schema(description = "短信签名")
@EqualsAndHashCode(callSuper = false)
public class SmsSign extends BaseIdEntity {

    @Schema(description = "签名名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String signName;

    @Schema(description = "签名来源", requiredMode = Schema.RequiredMode.REQUIRED)
    private Integer signSource;

    @Schema(description = "短信签名申请说明", requiredMode = Schema.RequiredMode.REQUIRED)
    private String remark;

    @Schema(description = "营业执照", requiredMode = Schema.RequiredMode.REQUIRED)
    private String businessLicense;

    @Schema(description = "授权委托书", requiredMode = Schema.RequiredMode.REQUIRED)
    private String license;

    /**
     * 0：审核中。
     * 1：审核通过。
     * 2：审核失败，请在返回参数Reason中查看审核失败原因。
     */
    @Schema(description = "签名审核状态")
    private Integer signStatus;

    @Schema(description = "审核备注")
    private String reason;

}
