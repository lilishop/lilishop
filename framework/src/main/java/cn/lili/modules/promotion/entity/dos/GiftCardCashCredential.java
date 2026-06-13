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
@Schema(description = "现金礼品卡卡密凭证")
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

    @Schema(description = "单卡面额")
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

    @Schema(description = "当前余额")
    private BigDecimal balance;

    /**
     * @see cn.lili.modules.promotion.entity.enums.GiftCardCashCredentialStatusEnum
     */
    @Schema(description = "凭证状态：UNISSUED/BOUND/VOIDED/EXPIRED")
    private String status;
}
