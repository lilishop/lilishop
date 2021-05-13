package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.List;

/**
 * 拼团视图对象
 *
 * @author paulG
 * @date 2020/10/28
 **/
@Data
public class PintuanVO extends Pintuan {

    private static final long serialVersionUID = 218582640653676201L;

    private List<PromotionGoods> promotionGoodsList;

}
