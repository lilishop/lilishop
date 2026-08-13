package cn.lili.modules.order.order.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dos.OrderPackageItem;
import cn.lili.modules.order.order.entity.enums.FulfillmentModeEnum;
import cn.lili.modules.system.entity.vo.Traces;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 分包裹详情VO
 *
 * @author Chopper
 * @since 2020-08-17 20:28
 */
@Data
@NoArgsConstructor
public class OrderPackageVO implements Serializable {

    private static final long serialVersionUID = 1810890757303309436L;

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

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "创建时间")
    private Date createTime;

    @Schema(description = "子订单包裹详情列表")
    private List<OrderPackageItem> orderPackageItemList;

    @Schema(description = "物流信息")
    private Traces traces;

    /**
     * @see cn.lili.modules.order.order.entity.enums.FulfillmentModeEnum
     */
    @Schema(description = "履约模式 WHOLE/SPLIT/NO_DELIVERY")
    private String fulfillmentMode;

    @Schema(description = "无需配送说明")
    private String deliveryRemark;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "本批次发货时间")
    private Date logisticsTime;

    @Schema(description = "是否需要查询快递轨迹")
    private Boolean logisticsRequired;

    public OrderPackageVO(OrderPackage orderPackage) {
        BeanUtil.copyProperties(orderPackage, this);
        if (this.logisticsTime == null) {
            this.logisticsTime = orderPackage.getLogisticsTime();
        }
        this.logisticsRequired = !FulfillmentModeEnum.NO_DELIVERY.name()
                .equals(CharSequenceUtil.blankToDefault(orderPackage.getFulfillmentMode(), FulfillmentModeEnum.SPLIT.name()))
                && CharSequenceUtil.isNotBlank(orderPackage.getLogisticsNo());
    }

}
