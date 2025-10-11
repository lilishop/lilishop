package cn.lili.modules.statistics.entity.vo;


import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 营业构成VO
 * @author Bulbasaur
 * @since 2025/08/25 7:07 下午
 */
@Data
public class BusinessCompositionDataVO {

    //订单分类构成
    @ApiModelProperty(value = "到店自提")
    private Double storeSelf;
    @ApiModelProperty(value = "快递发货")
    private Double express;
    @ApiModelProperty(value = "线上无需配送")
    private Double online;

    //营业收入
    @ApiModelProperty(value = "商品销售")
    private Double income;
    @ApiModelProperty(value = "运费")
    private Double freight;
    @ApiModelProperty(value = "商品返现（分销返佣）")
    private Double incomeBack;
    @ApiModelProperty(value = "商品销售+费用构成")
    private Double incomeComposition;

    //退款统计
    @ApiModelProperty(value = "退款订单笔数")
    private Long refundOrderNum;
    @ApiModelProperty(value = "退款金额")
    private Double refund;
    @ApiModelProperty(value = "退款率")
    private Double refundRate;

    //消费指标
    @ApiModelProperty(value = "支付金额")
    private Double pay;
    @ApiModelProperty(value = "折后笔单价")
    private Double price;
    @ApiModelProperty(value = "支付人数")
    private Long payNum;
    @ApiModelProperty(value = "折后客单价")
    private Double priceNum;





}
