package cn.lili.modules.appointment.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * 预约核销码视图
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@Schema(description = "预约核销码视图")
public class AppointmentVerificationCodeVO implements Serializable {

    @Schema(description = "核销码ID")
    private String id;

    @Schema(description = "核销码")
    private String code;

    @Schema(description = "序号")
    private Integer seqNo;

    @Schema(description = "状态 UNUSED/USED/VOIDED")
    private String status;

    @Schema(description = "核销服务人员ID")
    private String staffId;

    @Schema(description = "核销服务人员姓名")
    private String staffName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "核销时间")
    private Date verifiedTime;
}
