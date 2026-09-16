package cn.lili.modules.distribution.entity.dos;

import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 分销订单
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@Schema(description = "分销订单")
@TableName("li_distribution_order")
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class DistributionOrder extends BaseIdEntity {

    private static final long serialVersionUID = 501799944909496507L;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @Schema(description = "创建时间", hidden = true)
    private Date createTime;

    /**
     * @see DistributionOrderStatusEnum
     */
    @Schema(description = "分销订单状态")
    private String distributionOrderStatus;

    @Schema(description = "购买客户的id")
    private String memberId;

    @Schema(description = "购买客户的名称")
    private String memberName;

    @Schema(description = "一级分销员id")
    private String distributionId;

    @Schema(description = "一级分销员名称")
    private String distributionName;

    @Schema(description = "二级分销员id（邀请奖励受益人）")
    private String inviteDistributionId;

    @Schema(description = "二级分销员名称")
    private String inviteDistributionName;

    @Schema(description = "业绩归属分销员ID")
    private String performanceDistributorId;

    @Schema(description = "是否本行分销员自购")
    private Boolean selfPurchase;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "支付成功时间")
    private Date paymentTime;

    @Schema(description = "本明细佣金池金额")
    private BigDecimal commissionPool;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "解冻日期")
    private Date settleCycle;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "佣金结算时间")
    private Date settleTime;

    @TableField("rebate")
    @Schema(description = "一级分销佣金")
    private Double directCommission;

    @Schema(description = "二级邀请佣金")
    private Double inviteCommission;

    @Schema(description = "一级佣金退款冲回金额")
    private Double sellBackRebate;

    @Schema(description = "二级邀请佣金退款冲回金额")
    private Double inviteSellBackRebate;

    @Schema(description = "店铺id")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "子订单编号")
    private String orderItemSn;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "货品ID")
    private String skuId;

    @Schema(description = "规格")
    private String specs;

    @Schema(description = "图片")
    private String image;

    @Schema(description = "商品数量")
    private Integer num;

    @Schema(description = "退款商品数量")
    private Integer refundNum;

    public DistributionOrder(StoreFlow storeFlow) {
        distributionOrderStatus = DistributionOrderStatusEnum.NO_COMPLETED.name();
        memberId = storeFlow.getMemberId();
        memberName = storeFlow.getMemberName();
        directCommission = storeFlow.getDistributionRebate();
        storeId = storeFlow.getStoreId();
        storeName = storeFlow.getStoreName();
        orderSn = storeFlow.getOrderSn();
        orderItemSn = storeFlow.getOrderItemSn();
        goodsId = storeFlow.getGoodsId();
        goodsName = storeFlow.getGoodsName();
        skuId = storeFlow.getSkuId();
        specs = storeFlow.getSpecs();
        image = storeFlow.getImage();
        num = storeFlow.getNum();
        refundNum = 0;
        sellBackRebate = 0D;
    }
}
