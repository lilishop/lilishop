package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 分销客户关系设置
 *
 * @author pikachu
 */
@Data
public class DistributionCustomerSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionBindModeEnum
     */
    @Schema(description = "锁客模式")
    private String bindMode;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionPermanentBindModeEnum
     */
    @Schema(description = "永久锁客生效方式")
    private String permanentBindMode;

    @Schema(description = "永久锁客支付天数")
    private Integer permanentPaidDays;

    @Schema(description = "短期锁客有效天数")
    private Integer shortTermBindDays;
}
