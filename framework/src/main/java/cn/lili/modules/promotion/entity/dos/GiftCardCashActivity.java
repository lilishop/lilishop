package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
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
 * 礼品卡活动实体类（库存与规则；其下可多次制卡、多次发卡/发送）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_gift_card_cash_activity")
@Schema(description = "现金礼品卡活动")
public class GiftCardCashActivity extends BaseEntity {

    @Schema(description = "活动名称")
    private String giftCardName;

    /**
     * @see cn.lili.modules.promotion.entity.enums.GiftCardTypeEnum
     */
    @Schema(description = "礼品卡类型：CASH / PICKUP")
    private String cardType;

    @Schema(description = "单卡面额")
    private BigDecimal faceValue;

    @Schema(description = "有效期类型")
    private String validityType;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "固定到期时间")
    private Date validEndTime;

    @Schema(description = "激活后有效月数")
    private Integer validMonthsAfterActivate;

    @Schema(description = "总库存")
    private Integer stockTotal;

    @TableField(exist = false)
    @Schema(description = "制卡数（未发放凭证池，可导出卡密/用户自助绑定）")
    private Integer producedQuantity;

    @TableField(exist = false)
    @Schema(description = "发卡数（导入直接发放给会员）")
    private Integer issuedCardQuantity;

    @TableField(exist = false)
    @Schema(description = "剩余库存（总库存-已占用张数，制卡与发卡共用）")
    private Integer remainingStock;
}
