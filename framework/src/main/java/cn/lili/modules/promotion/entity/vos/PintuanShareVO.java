package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import lombok.Data;

import java.util.List;

/**
 * 拼图会员分享对象
 *
 * @author paulG
 * @since 2021/3/24
 **/
@Data
public class PintuanShareVO {

    private PromotionGoods promotionGoods;

    private List<PintuanMemberVO> pintuanMemberVOS;

}
