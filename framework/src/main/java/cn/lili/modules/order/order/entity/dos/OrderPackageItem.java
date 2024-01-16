package cn.lili.modules.order.order.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Data
@TableName("li_order_package_item")
@ApiModel(value = "订单分包裹详情")
@NoArgsConstructor
@AllArgsConstructor
public class OrderPackageItem extends BaseEntity {

    @ApiModelProperty(value = "包裹单号")
    private String packageNo;

    @ApiModelProperty(value = "订单编号")
    private String orderSn;

    @ApiModelProperty(value = "子订单编号")
    private String orderItemSn;

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "商品图片")
    private String thumbnail;

    @ApiModelProperty(value = "已发货数量")
    private Integer deliverNumber;

    @ApiModelProperty(value = "送货时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date logisticsTime;

}
