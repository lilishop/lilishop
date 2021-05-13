package cn.lili.modules.order.cart.entity.vo;

import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import lombok.Data;

import java.util.List;

/**
 * 满额活动VO
 *
 * @author Chopper
 * @date 2020-04-01 10:42 上午
 */
@Data
public class FullDiscountVO extends FullDiscount {

    private static final long serialVersionUID = -2330552735874105354L;
    /**
     * 促销关联的商品
     */
    private List<PromotionGoods> promotionGoodsList;

    private Integer number;

    public String notice() {
        StringBuilder stringBuffer = new StringBuilder();
        if (Boolean.TRUE.equals(this.getIsFreeFreight())) {
            stringBuffer.append("免运费 ");
        }
        if (Boolean.TRUE.equals(this.getIsFullMinus())) {
            stringBuffer.append("减").append(this.getFullMinus()).append("元 ");
        }
        if (Boolean.TRUE.equals(this.getIsFullRate())) {
            stringBuffer.append("打").append(this.getFullRate()).append("折 ");
        }
        return stringBuffer.toString();
    }

}
