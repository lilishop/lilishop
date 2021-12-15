package cn.lili.modules.statistics.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 首页统计内容
 *
 * @author Bulbasaur
 * @since 2020/12/22 14:23
 */
@Data
public class IndexStatisticsVO {

    @ApiModelProperty(value = "订单总数量")
    private Long orderNum;
    @ApiModelProperty(value = "商品总数量")
    private Long goodsNum;
    @ApiModelProperty(value = "会员总数量")
    private Long memberNum;
    @ApiModelProperty(value = "店铺总数量")
    private Long storeNum;

    /**
     * 流量概括
     */
    @ApiModelProperty(value = "今日访问数UV")
    private Integer todayUV;
    @ApiModelProperty(value = "昨日访问数UV")
    private Integer yesterdayUV;
    @ApiModelProperty(value = "前七日访问数UV")
    private Integer lastSevenUV;
    @ApiModelProperty(value = "三十日访问数UV")
    private Integer lastThirtyUV;

    /**
     * 今日信息概括
     */
    @ApiModelProperty(value = "今日订单数")
    private Long todayOrderNum;
    @ApiModelProperty(value = "今日下单金额")
    private Double todayOrderPrice;
    @ApiModelProperty(value = "今日新增会员数量")
    private Long todayMemberNum;
    @ApiModelProperty(value = "今日新增商品数量")
    private Long todayGoodsNum;
    @ApiModelProperty(value = "今日新增店铺数量")
    private Long todayStoreNum;
    @ApiModelProperty(value = "今日新增评论数量")
    private Long todayMemberEvaluation;
    @ApiModelProperty(value = "当前在线人数")
    private Long currentNumberPeopleOnline;
}
