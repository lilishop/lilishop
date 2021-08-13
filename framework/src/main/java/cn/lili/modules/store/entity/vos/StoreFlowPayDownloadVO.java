package cn.lili.modules.store.entity.vos;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 店铺流水下载
 * @author Bulbasaur
 * @date: 2021/8/13 4:14 下午
 *
 */
@Data
public class StoreFlowPayDownloadVO {

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "订单sn")
    private String orderSn;

    @ApiModelProperty(value = "店铺名称 ")
    private String storeName;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "销售量")
    private Integer num;

    @ApiModelProperty(value = "流水金额")
    private Double finalPrice;

    @ApiModelProperty(value = "平台收取交易佣金")
    private Double commissionPrice;

    @ApiModelProperty(value = "平台优惠券 使用金额")
    private Double siteCouponPrice;

    @ApiModelProperty(value = "单品分销返现支出")
    private Double distributionRebate;

    @ApiModelProperty(value = "积分活动商品结算价格")
    private Double pointSettlementPrice;

    @ApiModelProperty(value = "砍价活动商品结算价格")
    private Double kanjiaSettlementPrice;

    @ApiModelProperty(value = "最终结算金额")
    private Double billPrice;
}
