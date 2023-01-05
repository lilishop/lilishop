package cn.lili.modules.distribution.entity.dos;

import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销订单
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@ApiModel(value = "分销订单")
@TableName("li_distribution_order")
@NoArgsConstructor
public class DistributionOrder extends BaseIdEntity {

    private static final long serialVersionUID = 501799944909496507L;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    /**
     * @see DistributionOrderStatusEnum
     */
    @ApiModelProperty(value = "分销订单状态")
    private String distributionOrderStatus;
    @ApiModelProperty(value = "购买会员的id")
    private String memberId;
    @ApiModelProperty(value = "购买会员的名称")
    private String memberName;
    @ApiModelProperty(value = "分销员id")
    private String distributionId;
    @ApiModelProperty(value = "分销员名称")
    private String distributionName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "解冻日期")
    private Date settleCycle;
    @ApiModelProperty(value = "提成金额")
    private Double rebate;
    @ApiModelProperty(value = "退款金额")
    private Double sellBackRebate;
    @ApiModelProperty(value = "店铺id")
    private String storeId;
    @ApiModelProperty(value = "店铺名称")
    private String storeName;
    @ApiModelProperty(value = "订单编号")
    private String orderSn;
    @ApiModelProperty(value = "子订单编号")
    private String orderItemSn;
    @ApiModelProperty(value = "商品ID")
    private String goodsId;
    @ApiModelProperty(value = "商品名称")
    private String goodsName;
    @ApiModelProperty(value = "货品ID")
    private String skuId;
    @ApiModelProperty(value = "规格")
    private String specs;
    @ApiModelProperty(value = "图片")
    private String image;
    @ApiModelProperty(value = "商品数量")
    private Integer num;

    public DistributionOrder(StoreFlow storeFlow) {
        distributionOrderStatus = DistributionOrderStatusEnum.WAIT_BILL.name();
        memberId = storeFlow.getMemberId();
        memberName = storeFlow.getMemberName();
        rebate = storeFlow.getDistributionRebate();
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
    }

}