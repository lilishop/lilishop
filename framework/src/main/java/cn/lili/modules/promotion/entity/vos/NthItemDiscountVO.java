package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.NthItemDiscount;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;

import java.util.List;

@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
public class NthItemDiscountVO extends NthItemDiscount {

    private static final long serialVersionUID = 1L;

    private List<PromotionGoods> promotionGoodsList;

    public NthItemDiscountVO(NthItemDiscount nthItemDiscount) {
        BeanUtils.copyProperties(nthItemDiscount, this);
    }
}
