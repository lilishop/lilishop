package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 礼品卡卡密凭证实体类（平台制卡产物：卡号/卡密/库存，未与会员绑定时无余额消费）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_gift_card_cash_credential")
@Schema(description = "礼品卡卡密凭证（现金卡、提货卡）")
public class GiftCardCashCredential extends BaseEntity {

    @Schema(description = "活动ID")
    private String activityId;

    @Schema(description = "制卡批次ID")
    private String createBatchId;

    @Schema(description = "发卡批次ID")
    private String issueBatchId;

    @Schema(description = "卡号")
    private String cardNo;

    @Schema(description = "卡密")
    private String cardSecret;

    @Schema(description = "卡类型快照：CASH 现金卡、PICKUP 提货卡；由活动写入，不允许买家指定")
    private String cardType;

    @Schema(description = "成功兑换的 li_trade.sn；一次核销后取消不恢复，重试返回原交易")
    private String pickupTradeSn;

    @Schema(description = "单卡面额（人民币元）；现金卡保留发行面额，提货卡固定为零")
    private BigDecimal faceValue;

    @Schema(description = "绑定会员ID")
    private String memberId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "绑定时间（用户兑换/领卡时写入）")
    private Date bindTime;

    @Schema(description = "激活会员ID")
    private String activateMemberId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "激活时间（激活后方可使用）")
    private Date activateTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "卡到期时间（按活动有效期规则计算）")
    private Date expireTime;

    @Schema(description = "当前余额（人民币元）")
    private BigDecimal balance;

    @Schema(description = "凭证状态：UNISSUED 未发卡、BOUND 已绑定、VOIDED 作废、EXPIRED 过期、REDEEMED 提货已用；提货已用不可恢复", allowableValues = {"UNISSUED", "BOUND", "VOIDED", "EXPIRED", "REDEEMED"})
    private String status;
}
