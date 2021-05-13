package cn.lili.modules.system.entity.dos;


import cn.lili.common.utils.StringUtils;
import cn.lili.modules.system.entity.plugin.InstantDelivery.dada.enums.DadaOrderStatusEnum;
import cn.lili.modules.system.entity.plugin.InstantDelivery.dada.vo.DdOrderBackVO;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 即时配送日志
 *
 * @author pikachu
 * @date 2020/12/01 15:58
 */

@Data
@Entity
@Table(name = "li_instant_delivery_log")
@TableName("li_instant_delivery_log")
@ApiModel(value = "即时配送日志")
@AllArgsConstructor
@NoArgsConstructor
public class InstantDeliveryLog {

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;
    /**
     * 即时配送订单号
     */
    @ApiModelProperty(value = "即时配送订单号")
    private String deliveryOrderSn;
    /**
     * 商城订单号
     */
    @ApiModelProperty(value = "商城订单号")
    private String orderSn;
    /**
     * 即时配送订单状态
     *
     * @see DadaOrderStatusEnum
     */
    @ApiModelProperty(value = "即时配送订单状态")
    private String deliveryOrderStatus;
    /**
     * 配送员ID
     */
    @ApiModelProperty(value = "即时配送骑手编号")
    private String distributorNumber;
    /**
     * 配送员姓名，接单以后会存储
     */
    @ApiModelProperty(value = "配送员姓名，接单以后会存储")
    private String distributorName;
    /**
     * 配送员手机号，接单以后存储
     */
    @ApiModelProperty(value = "配送员手机号，接单以后存储")
    private String distributorMobile;

    /**
     * 订单取消原因,其他状态下默认值为空字符串
     */
    @ApiModelProperty(value = "订单取消原因,其他状态下默认值为空字符串")
    private String cancelReason;

    public InstantDeliveryLog(DdOrderBackVO ddOrderBackVO) {
        this.setCancelReason(ddOrderBackVO.getCancelReason());
        this.setOrderSn(ddOrderBackVO.getOrderId());
        this.setDeliveryOrderSn(ddOrderBackVO.getClientId());
        if (!StringUtils.isEmpty(ddOrderBackVO.getDmMobile())) {
            this.setDistributorMobile(ddOrderBackVO.getDmMobile());
        }
        if (!StringUtils.isEmpty(ddOrderBackVO.getDmName())) {
            this.setDistributorName(ddOrderBackVO.getDmName());
        }
        if (ddOrderBackVO.getDmId() != null) {
            this.setDistributorNumber(ddOrderBackVO.getDmId().toString());
        }
        this.setDeliveryOrderStatus(DadaOrderStatusEnum.getText(100));
    }


}