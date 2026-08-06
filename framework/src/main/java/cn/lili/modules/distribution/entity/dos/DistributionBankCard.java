package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

/**
 * 分销员银行卡
 *
 * @author Bulbasaur
 * @since 2026-08-06
 */
@Data
@TableName("li_distribution_bank_card")
@Schema(description = "分销员银行卡")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class DistributionBankCard extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "收款人姓名")
    private String holderName;

    @Schema(description = "银行名称")
    private String bankName;

    @Schema(description = "银行卡号")
    private String cardNo;

    @Schema(description = "是否默认卡")
    private Boolean isDefault;
}
