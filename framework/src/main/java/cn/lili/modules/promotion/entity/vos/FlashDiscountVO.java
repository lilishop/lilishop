package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.FlashDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.List;

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class FlashDiscountVO extends FlashDiscount {

    private static final long serialVersionUID = 1L;

    private List<PromotionGoods> promotionGoodsList;

    public FlashDiscountVO(FlashDiscount flashDiscount) {
        BeanUtils.copyProperties(flashDiscount, this);
    }
}
