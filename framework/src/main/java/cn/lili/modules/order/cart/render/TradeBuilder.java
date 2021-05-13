package cn.lili.modules.order.cart.render;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.service.CartService;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.service.TradeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

/**
 * TradeBuilder
 *
 * @author Chopper
 * @date2020-04-01 9:47 下午
 */
@Service
@Slf4j
public class TradeBuilder {
    //购物车渲染
    @Autowired
    private List<CartRenderStep> cartRenderSteps;
    //交易
    @Autowired
    private TradeService tradeService;

    /**
     * 渲染整比交易
     */
    int[] defaultRender = {0, 1, 2, 4, 5, 6, 7};

    /**
     * 购物车购物车渲染
     */
    int[] cartRender = {0, 1, 2, 5};

    /**
     * 0-> 校验商品 1-》 满优惠渲染 2->渲染优惠 3->优惠券渲染 4->计算运费 5->计算价格 6->分销渲染 7->扩展操作
     */
    @Autowired
    private CartService cartService;

    /**
     * 构造购物车
     *
     * @param checkedWay 购物车类型
     * @return 购物车展示信息
     */
    public TradeDTO buildCart(CartTypeEnum checkedWay) {
        TradeDTO tradeDTO = cartService.readDTO(checkedWay);

        //购物车需要将交易中的优惠券取消掉
        if (checkedWay.equals(CartTypeEnum.CART)) {
            tradeDTO.setStoreCoupons(null);
            tradeDTO.setPlatformCoupon(null);
        }

        //按照计划进行渲染
        for (int index : cartRender) {
            try {
                cartRenderSteps.get(index).render(tradeDTO);
            } catch (Exception e) {
                log.error("购物车渲染异常：", e);
            }
        }
        return tradeDTO;
    }

    /**
     * 构造一笔交易
     *
     * @param checkedWay 购物车类型
     * @return 购物车展示信息
     */
    public TradeDTO buildTrade(CartTypeEnum checkedWay) {
        TradeDTO tradeDTO = cartService.readDTO(checkedWay);
        List<CartSkuVO> collect = tradeDTO.getSkuList().parallelStream().filter(i -> Boolean.TRUE.equals(i.getChecked())).collect(Collectors.toList());
        if (checkedWay.equals(CartTypeEnum.PINTUAN)) {
            for (CartSkuVO cartSkuVO : collect) {
                cartSkuVO.setPintuanId("");
            }
        }
        tradeDTO.setSkuList(collect);
        //按照计划进行渲染
        for (int index : defaultRender) {
            cartRenderSteps.get(index).render(tradeDTO);
        }
        List<CartVO> cartVOList = new ArrayList<>();
        for (CartVO i : tradeDTO.getCartList()) {
            i.setSkuList(i.getSkuList().stream().filter(j -> Boolean.TRUE.equals(j.getChecked())).collect(Collectors.toList()));
            cartVOList.add(i);
        }
        tradeDTO.setCartList(cartVOList);
        return tradeDTO;
    }

    /**
     * 创建一笔交易
     *
     * @param checkedWay    购物车类型
     * @param parentOrderSn 是否为其他订单下的订单，如果是则为依赖订单的sn，否则为空
     * @return 交易信息
     */
    public Trade createTrade(CartTypeEnum checkedWay, String parentOrderSn) {
        TradeDTO tradeDTO = this.buildTrade(checkedWay);
        tradeDTO.setParentOrderSn(parentOrderSn);
        return tradeService.createTrade(tradeDTO);
    }
}
