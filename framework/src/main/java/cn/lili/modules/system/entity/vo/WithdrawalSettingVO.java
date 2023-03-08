package cn.lili.modules.system.entity.vo;

import io.swagger.annotations.ApiModelProperty;
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
    @ApiModelProperty(value = "minPrice")
    private Double minPrice;
    /**
     * 提现手续费
     */
    @ApiModelProperty(value = "提现手续费")
    private Double fee;
    /**
     * 提现类型 WECHAT\ALI
     */
    @ApiModelProperty(value = "提现类型 WECHAT、ALI")
    private String type;

}
