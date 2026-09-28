package cn.lili.modules.appointment.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 预约订单扩展，对应 {@code li_appointment_order_ext}
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_appointment_order_ext")
@Schema(description = "预约订单扩展")
public class AppointmentOrderExt extends BaseEntity {

    private String orderSn;
    private String goodsId;
    private String skuId;
    private String storeId;
    private String memberId;
    private String serviceMode;
    private String slotStockId;
    private Date slotDate;
    private String slotStartTime;
    private String slotEndTime;
    private Integer quantity;
    private String contactName;
    private String contactPhone;
    private String serviceAddressSnapshot;
    private String formAnswerJson;
    private String cancelPolicySnapshot;
    private Boolean workOrderVisibleSnapshot;
    private Double refundedAmount;
}
