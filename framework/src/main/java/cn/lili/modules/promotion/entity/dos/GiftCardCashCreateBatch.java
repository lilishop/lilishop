package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 现金礼品卡制卡批次实体类
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@NoArgsConstructor
@TableName("li_gift_card_cash_create_batch")
@Schema(description = "现金礼品卡制卡批次")
public class GiftCardCashCreateBatch extends BaseEntity {

    @Schema(description = "所属礼品卡活动ID")
    private String activityId;

    @Schema(description = "本批次制卡张数")
    private Integer quantity;

    @TableField("batch_name")
    @Schema(description = "本批备注")
    private String batchRemark;

    @TableField("create_user_name")
    @Schema(description = "制卡人")
    private String createUserName;

    @TableField("create_user_mobile")
    @Schema(description = "制卡人手机号")
    private String createUserMobile;

}
