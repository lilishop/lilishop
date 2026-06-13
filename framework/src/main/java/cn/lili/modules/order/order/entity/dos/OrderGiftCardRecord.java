package cn.lili.modules.order.order.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * 订单礼品卡使用流水实体类（记录抵扣、退款、取消、激活等余额变动）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@TableName("li_order_gift_card_record")
@Schema(description = "礼品卡使用记录")
public class OrderGiftCardRecord extends BaseEntity {

    @Schema(description = "订单号")
    private String orderSn;

    @Schema(description = "交易号")
    private String tradeSn;

    @Schema(description = "会员ID")
    private String memberId;

    @Schema(description = "礼品卡凭证ID")
    private String credentialId;

    @Schema(description = "礼品卡卡号")
    private String cardNo;

    @Schema(description = "本次抵扣金额（兼容历史字段，扣减场景写入）")
    private BigDecimal deductAmount;

    @Schema(description = "变动金额（正数增加，负数减少）")
    private BigDecimal changeAmount;

    @Schema(description = "变动后余额")
    private BigDecimal balanceAfter;

    @Schema(description = "流水方向：INCREASE/DECREASE")
    private String flowType;

    @Schema(description = "业务类型：ORDER_DEDUCT/ORDER_REFUND/ORDER_CANCEL/CARD_ACTIVATE")
    private String bizType;

    @Schema(description = "业务单号（售后单号/订单号等）")
    private String bizSn;
}
