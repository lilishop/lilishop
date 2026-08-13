package cn.lili.modules.order.order.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

@Data
@TableName("li_order_package")
@Schema(description = "订单包裹")
@NoArgsConstructor
@AllArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class OrderPackage extends BaseEntity {

    @Schema(description = "包裹单号")
    private String packageNo;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "发货单号")
    private String logisticsNo;

    @Schema(description = "物流公司CODE")
    private String logisticsCode;

    @Schema(description = "物流公司名称")
    private String logisticsName;

    @Schema(description = "收件人手机")
    private String consigneeMobile;

    @Schema(description = "状态")
    private String status;

    /**
     * @see cn.lili.modules.order.order.entity.enums.FulfillmentModeEnum
     */
    @Schema(description = "履约模式 WHOLE/SPLIT/NO_DELIVERY")
    private String fulfillmentMode;

    @Schema(description = "无需配送说明")
    private String deliveryRemark;

    @Schema(description = "本批次发货时间")
    private java.util.Date logisticsTime;

}
