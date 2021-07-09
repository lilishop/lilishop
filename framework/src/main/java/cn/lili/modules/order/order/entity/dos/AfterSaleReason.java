package cn.lili.modules.order.order.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;

/**
 * 售后原因
 *
 * @author Bulbasaur
 * @date: 2021/7/9 1:39 上午
 */
@Data
@Entity
@Table(name = "li_after_sale_reason")
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
