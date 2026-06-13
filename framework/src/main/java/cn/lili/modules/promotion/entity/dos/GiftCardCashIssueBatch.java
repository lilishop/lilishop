package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 现金礼品卡发卡/发送记录实体类
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_gift_card_cash_issue_batch")
@Schema(description = "现金礼品卡发卡记录")
public class GiftCardCashIssueBatch extends BaseEntity {

    @Schema(description = "活动ID")
    private String activityId;

    @Schema(description = "发卡人")
    private String issueUserName;

    @Schema(description = "发卡人手机号")
    private String issueUserMobile;

    @Schema(description = "本次发卡总张数")
    private Integer totalCards;

    @Schema(description = "导入有效行数")
    private Integer totalRows;
}