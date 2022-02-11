package cn.lili.modules.order.cart.render.impl;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * 佣金计算
 *
 * @author Chopper
 * @see CartVO
 */
@Service
public class CheckedFilterRender implements CartRenderStep {

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.CHECKED_FILTER;
    }

    @Override
    public void render(TradeDTO tradeDTO) {
        //将购物车到sku未选择信息过滤
        List<CartSkuVO> collect = tradeDTO.getSkuList().stream().filter(i -> Boolean.TRUE.equals(i.getChecked())).collect(Collectors.toList());
        tradeDTO.setSkuList(collect);

        //购物车信息过滤
        List<CartVO> cartVOList = new ArrayList<>();
        //循环购物车信息
        for (CartVO cartVO : tradeDTO.getCartList()) {
            //如果商品选中，则加入到对应购物车
            cartVO.setSkuList(cartVO.getCheckedSkuList());
            cartVOList.add(cartVO);
        }
        tradeDTO.setCartList(cartVOList);
    }


}
