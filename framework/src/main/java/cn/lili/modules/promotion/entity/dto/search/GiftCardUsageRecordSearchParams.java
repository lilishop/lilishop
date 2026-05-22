package cn.lili.modules.promotion.entity.dto.search;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 礼品卡使用记录分页查询条件
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "礼品卡使用记录分页查询")
public class GiftCardUsageRecordSearchParams {

    @Schema(description = "订单号")
    private String orderSn;

    @Schema(description = "交易号")
    private String tradeSn;

    @Schema(description = "会员ID")
    private String memberId;

    @Schema(description = "会员手机号（精确）")
    private String memberMobile;

    @Schema(description = "客户名称（昵称/用户名模糊）")
    private String memberName;

    @Schema(description = "礼品卡卡号（模糊）")
    private String cardNo;

    @Schema(description = "流水方向：INCREASE/DECREASE")
    private String flowType;

    @Schema(description = "业务类型：ORDER_DEDUCT/ORDER_REFUND/ORDER_CANCEL/CARD_ACTIVATE")
    private String bizType;

    @Schema(description = "交易类型：ORDER_DEDUCT/ORDER_REFUND/ORDER_CANCEL")
    private String transactionType;

    @Schema(description = "业务单号（售后单号/订单号等）")
    private String bizSn;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "交易时间开始")
    private Date transactionTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "交易时间结束")
    private Date transactionTimeEnd;
}
