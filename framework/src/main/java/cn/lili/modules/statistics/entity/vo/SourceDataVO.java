package cn.lili.modules.statistics.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 收款构成
 *
 * @author Bulbasaur
 * @since 2025/08/25 7:07 下午
 */
@Data
public class SourceDataVO {

    @ApiModelProperty(value = "支付方式")
    private String payType;
    @ApiModelProperty(value = "收款合计")
    private Double total;
    @ApiModelProperty(value = "营业收入")
    private Double income;
    @ApiModelProperty(value = "新增储值金额")
    private Double recharge;



}
