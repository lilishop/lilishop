package cn.lili.modules.system.entity.vo;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

/**
 * 用户提现设置VO
 *
 * @author Bulbasaur
 * @since 2023/3/1
 */
@Data
public class WithdrawalSettingVO {

    /**
     * 提现最低金额
     */
    @Schema(description = "minPrice")
    private Double minPrice;
    /**
     * 提现手续费
     */
    @Schema(description = "提现手续费")
    private Double fee;
    /**
     * 提现类型 WECHAT\ALI
     */
    @Schema(description = "提现类型 WECHAT、ALI")
    private String type;

    /**
     * 提现规则说明
     */
    @Schema(description = "提现规则说明")
    private String withdrawRule;

}
