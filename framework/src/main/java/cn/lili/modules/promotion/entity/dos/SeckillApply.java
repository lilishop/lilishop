package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 秒杀活动申请实体类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@Data
@TableName("li_seckill_apply")
@Schema(description = "秒杀活动申请")
@EqualsAndHashCode(callSuper = false)
public class SeckillApply extends BaseEntity {

    private static final long serialVersionUID = 5440164641970820989L;

    @Schema(description = "活动id", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotNull(message = "活动id参数不能为空")
    @Min(value = 0, message = "活动id参数异常")
    private String seckillId;

    @Schema(description = "时刻")
    @NotNull(message = "时刻参数不能为空")
    private Integer timeLine;

    @Schema(description = "skuID")
    @NotNull(message = "skuId参数不能为空")
    @Min(value = 0, message = "skuID参数异常")
    private String skuId;

    @Schema(description = "商品名称")
    @NotEmpty(message = "商品名称参数不能为空")
    private String goodsName;

    @Schema(description = "商家id")
    private String storeId;

    @Schema(description = "商家名称")
    private String storeName;

    @Schema(description = "价格")
    @NotNull(message = "价格参数不能为空")
    @Min(value = 0, message = "价格参数不能小于0")
    private Double price;

    @Schema(description = "促销数量")
    @NotNull(message = "促销数量参数不能为空")
    @Min(value = 0, message = "促销数量数不能小于0")
    private Integer quantity;

    /**
     * @see PromotionsApplyStatusEnum
     */
    @Schema(description = "APPLY(\"申请\"), PASS(\"通过\"), REFUSE(\"拒绝\")")
    private String promotionApplyStatus;

    @Schema(description = "驳回原因")
    private String failReason;

    @Schema(description = "已售数量")
    private Integer salesNum;

    @Schema(description = "商品原始价格")
    private Double originalPrice;


}