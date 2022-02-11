package cn.lili.modules.payment.kit.plugin.wechat.model;

import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;


/**
 * 统一下单-优惠功能
 *
 * @author Chopper
 * @since 2020/12/17 17:46
 */
@Data
@Accessors(chain = true)
public class Detail {
    /**
     * 订单原价
     */
    private int cost_price;
    /**
     * 商品小票ID
     */
    private String invoice_id;
    /**
     * 单品列表
     */
    private List<GoodsDetail> goods_detail;
}
