package cn.lili.modules.appointment.entity.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * 预约结算辅助参数
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Data
@Schema(description = "预约结算参数")
public class AppointmentCheckoutDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @NotBlank
    @Schema(description = "SKU ID")
    private String skuId;

    @NotBlank
    @Schema(description = "时段库存ID")
    private String slotStockId;

    @NotBlank
    @Schema(description = "服务方式 IN_STORE/ON_SITE")
    private String serviceMode;

    @NotNull
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "预约日期")
    private Date slotDate;

    @NotBlank
    @Schema(description = "时段开始")
    private String slotStartTime;

    @NotBlank
    @Schema(description = "时段结束")
    private String slotEndTime;

    @Schema(description = "联系人")
    private String contactName;

    @Schema(description = "联系电话")
    private String contactPhone;

    @Schema(description = "上门服务地址快照")
    private String serviceAddressSnapshot;

    @Schema(description = "上门地址经度，GCJ-02")
    private Double serviceLongitude;

    @Schema(description = "上门地址纬度，GCJ-02")
    private Double serviceLatitude;

    @Schema(description = "补充表单答案 JSON")
    private String formAnswerJson;
}
