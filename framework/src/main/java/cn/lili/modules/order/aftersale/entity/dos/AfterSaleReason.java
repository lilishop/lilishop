package cn.lili.modules.order.aftersale.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;

/**
 * 售后原因
 *
 * @author Bulbasaur
 * @since 2021/7/9 1:39 上午
 */
@Data
@TableName("li_after_sale_reason")
@ApiModel(value = "售后原因")
public class AfterSaleReason extends BaseEntity {

    @NotNull
    @ApiModelProperty(value = "售后原因")
    private String reason;

    /**
     * @see cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum
     */
    @ApiModelProperty(value = "原因类型", allowableValues = "CANCEL,RETURN_GOODS,RETURN_MONEY,COMPLAIN")
    @NotNull
    private String serviceType;

}
