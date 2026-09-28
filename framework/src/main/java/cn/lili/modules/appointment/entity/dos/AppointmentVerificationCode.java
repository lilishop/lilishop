package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 预约核销码，对应 {@code li_appointment_verification_code}。
 * 一单按购买数量生成 N 条，店内按码核销。
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_verification_code")
@Schema(description = "预约核销码")
public class AppointmentVerificationCode extends BaseEntity {

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "子订单编号")
    private String orderItemSn;

    @Schema(description = "店铺 ID")
    private String storeId;

    @Schema(description = "买家会员 ID")
    private String memberId;

    @Schema(description = "核销码，店内唯一")
    private String code;

    @Schema(description = "序号 1..N，对应购买件数")
    private Integer seqNo;

    @Schema(description = "UNUSED/USED/VOIDED")
    private String status;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "核销时间")
    private Date verifiedTime;

    @Schema(description = "核销操作人（商家账号）")
    private String verifiedByUserId;

    @Schema(description = "实际服务人员 ID")
    private String staffId;
}
