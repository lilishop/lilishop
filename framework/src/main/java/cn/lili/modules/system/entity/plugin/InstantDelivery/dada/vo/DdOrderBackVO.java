package cn.lili.modules.system.entity.plugin.InstantDelivery.dada.vo;

import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.databind.annotation.JsonNaming;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.ToString;

/**
 * @author 86133
 * @description: pikachu
 * @date 2020/9/1215:35
 */
@ApiModel(description = "达达订单回调参数")
@Data
@ToString
@JsonNaming(value = PropertyNamingStrategy.SnakeCaseStrategy.class)
public class DdOrderBackVO {
    @ApiModelProperty(value = "达达运单号", required = false)
    private String clientId;

    @ApiModelProperty(value = "交易编号", required = true)
    private String orderId;

    @ApiModelProperty(value = "订单状态 待接单＝1,待取货＝2,配送中＝3,已完成＝4,已取消＝5, 指派单=8,妥投异常之物品返回中=9, 妥投异常之物品返回完成=10, 骑士到店=100,创建达达运单失败=1000", required = true)
    private Integer orderStatus;

    @ApiModelProperty(value = "订单取消原因,其他状态下默认值为空字符串", required = true)
    private String cancelReason;

    @ApiModelProperty(value = "订单取消原因来源(1:达达配送员取消；2:商家主动取消；3:系统或客服取消；0:默认值)", required = true)
    private Integer cancelFrom;

    @ApiModelProperty(value = "更新时间", required = true)
    private Long updateTime;

    @ApiModelProperty(value = "加密串", required = true)
    private String signature;

    @ApiModelProperty(value = "达达配送员id，接单以后会传", required = false)
    private Integer dmId;

    @ApiModelProperty(value = "配送员姓名，接单以后会传", required = false)
    private String dmName;

    @ApiModelProperty(value = "配送员手机号，接单以后会传", required = false)
    private String dmMobile;

}
