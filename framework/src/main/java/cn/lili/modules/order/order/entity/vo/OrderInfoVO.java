package cn.lili.modules.order.order.entity.vo;

import cn.lili.modules.order.order.entity.dos.Order;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.beans.BeanUtils;

/**
 * 订单详情中的订单展示对象，保留原响应结构并承载从兑换流水读取的提货卡关联。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "订单详情展示信息")
public class OrderInfoVO extends Order {

    @Schema(description = "提货卡凭据 ID，从原兑换流水读取；仅用于展示，非订单表字段")
    private String pickupCardId;

    /**
     * 复制订单快照并补充提货卡关联，不修改订单实体。
     * @param order 持久化订单
     * @param pickupCardId 原兑换流水中的凭据 ID，非提货订单为空
     */
    public OrderInfoVO(Order order, String pickupCardId) {
        BeanUtils.copyProperties(order, this);
        this.setPriceDetail(order.getPriceDetail());
        this.pickupCardId = pickupCardId;
    }
}
