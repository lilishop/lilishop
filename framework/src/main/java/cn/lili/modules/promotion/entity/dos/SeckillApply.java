package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;

/**
 * 秒杀活动申请实体类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@Data
@TableName("li_seckill_apply")
@ApiModel(value = "秒杀活动申请")
public class SeckillApply extends BaseEntity {

    private static final long serialVersionUID = 5440164641970820989L;

    @ApiModelProperty(value = "活动id", required = true)
    @NotNull(message = "活动id参数不能为空")
    @Min(value = 0, message = "活动id参数异常")
    private String seckillId;

    @ApiModelProperty(value = "时刻")
    @NotNull(message = "时刻参数不能为空")
    private Integer timeLine;

    @ApiModelProperty(value = "skuID")
    @NotNull(message = "skuId参数不能为空")
    @Min(value = 0, message = "skuID参数异常")
    private String skuId;

    @ApiModelProperty(value = "商品名称")
    @NotEmpty(message = "商品名称参数不能为空")
    private String goodsName;

    @ApiModelProperty(value = "商家id")
    private String storeId;

    @ApiModelProperty(value = "商家名称")
    private String storeName;

    @ApiModelProperty(value = "价格")
    @NotNull(message = "价格参数不能为空")
    @Min(value = 0, message = "价格参数不能小于0")
    private Double price;

    @ApiModelProperty(value = "促销数量")
    @NotNull(message = "促销数量参数不能为空")
    @Min(value = 0, message = "促销数量数不能小于0")
    private Integer quantity;

    /**
     * @see PromotionsApplyStatusEnum
     */
    @ApiModelProperty(value = "APPLY(\"申请\"), PASS(\"通过\"), REFUSE(\"拒绝\")")
    private String promotionApplyStatus;

    @ApiModelProperty(value = "驳回原因")
    private String failReason;

    @ApiModelProperty(value = "已售数量")
    private Integer salesNum;

    @ApiModelProperty(value = "商品原始价格")
    private Double originalPrice;


}