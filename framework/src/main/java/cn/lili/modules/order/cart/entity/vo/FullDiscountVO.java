package cn.lili.modules.order.cart.entity.vo;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.List;

/**
 * 满额活动VO
 *
 * @author Chopper
 * @since 2020-04-01 10:42 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@NoArgsConstructor
public class FullDiscountVO extends FullDiscount {

    private static final long serialVersionUID = -2330552735874105354L;

    /**
     * 促销关联的商品
     */
    private List<PromotionGoods> promotionGoodsList;

    /**
     * 赠品skuId
     */
    private String giftSkuId;

    /**
     * 赠品名称
     */
    private String giftSkuName;

    /**
     * 赠品路径
     */
    private String giftSkuThumbnail;


    public FullDiscountVO(FullDiscount fullDiscount) {
        BeanUtils.copyProperties(fullDiscount, this);
    }

    public String notice() {
        StringBuilder stringBuffer = new StringBuilder();
        if (Boolean.TRUE.equals(this.getFullMinusFlag())) {
            stringBuffer.append(" 减").append(this.getFullMinus()).append("元 ");
        }
        if (Boolean.TRUE.equals(this.getFullRateFlag())) {
            stringBuffer.append(" 打").append(this.getFullRate()).append("折 ");
        }

        if (Boolean.TRUE.equals(this.getFreeFreightFlag())) {
            stringBuffer.append(" 免运费 ");
        }

        if (Boolean.TRUE.equals(this.getPointFlag())) {
            stringBuffer.append(" 赠").append(this.getPoint()).append("积分 ");
        }
        if (Boolean.TRUE.equals(this.getCouponFlag())) {
            stringBuffer.append(" 赠").append("优惠券 ");
        }
        if (Boolean.TRUE.equals(this.getGiftFlag() && CharSequenceUtil.isNotEmpty(giftSkuName))) {
            stringBuffer.append(" 赠品[").append(giftSkuName).append("]");
        }

        return stringBuffer.toString();
    }

}
