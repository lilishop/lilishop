package cn.lili.modules.order.cart.render;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
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
 * 交易构造&&创建
 *
 * @author Chopper
 * @since2020-04-01 9:47 下午
 */
@Service
@Slf4j
public class TradeBuilder {

    /**
     * 购物车渲染步骤
     */
    @Autowired
    private List<CartRenderStep> cartRenderSteps;
    /**
     * 交易
     */
    @Autowired
    private TradeService tradeService;
    /**
     * 购物车业务
     */
    @Autowired
    private CartService cartService;


    /**
     * 渲染整比交易
     * 校验商品 》 满优惠渲染  》  渲染优惠  》  优惠券渲染  》 计算运费  》  计算价格  》  分销渲染  》 订单SN初始化
     */
    RenderStepEnums[] defaultRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.FULL_DISCOUNT,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.COUPON,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.CART_PRICE,
            RenderStepEnums.DISTRIBUTION,
            RenderStepEnums.CART_SN
    };

    /**
     * 单个商品优惠，不需要渲染满减优惠
     * 用于特殊场景：例如积分商品，拼团商品，虚拟商品等等
     */
    RenderStepEnums[] singleRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.SKU_FREIGHT,
            RenderStepEnums.CART_PRICE,
            RenderStepEnums.DISTRIBUTION,
            RenderStepEnums.CART_SN};

    /**
     * 购物车购物车渲染
     * 校验商品 》 满优惠渲染  》  渲染优惠  》计算价格
     */
    RenderStepEnums[] cartRender = {
            RenderStepEnums.CHECK_DATA,
            RenderStepEnums.FULL_DISCOUNT,
            RenderStepEnums.SKU_PROMOTION,
            RenderStepEnums.CART_PRICE};


    /**
     * 构造购物车
     * 购物车与结算信息不一致的地方主要是优惠券计算和运费计算，其他规则都是一致都
     *
     * @param checkedWay 购物车类型
     * @return 购物车展示信息
     */
    public TradeDTO buildCart(CartTypeEnum checkedWay) {
        //读取对应购物车的商品信息
        TradeDTO tradeDTO = cartService.readDTO(checkedWay);

        //购物车需要将交易中的优惠券取消掉
        if (checkedWay.equals(CartTypeEnum.CART)) {
            tradeDTO.setStoreCoupons(null);
            tradeDTO.setPlatformCoupon(null);
        }

        //按照计划进行渲染
        for (RenderStepEnums step : cartRender) {
            for (CartRenderStep render : cartRenderSteps) {
                try {
                    if (render.step().equals(step)) {
                        render.render(tradeDTO);
                    }
                } catch (Exception e) {
                    log.error("购物车{}渲染异常：", render.getClass(), e);
                }
            }
        }
        return tradeDTO;
    }

    /**
     * 构造一笔交易
     * 1.从缓存中读取交易数据
     * 2.从购物车列表中筛选出已选择的SKU列表存入交易中
     * 3.渲染整个交易（0-> 校验商品 1-》 满优惠渲染 2->渲染优惠 3->优惠券渲染 4->计算运费 5->计算价格 6->分销渲染 7->其他渲染）
     * 4.将已选择的购物车列表存入交易中
     *
     * @param checkedWay 购物车类型
     * @return 购物车展示信息
     */
    public TradeDTO buildTrade(CartTypeEnum checkedWay) {
        //读取对应购物车的商品信息
        TradeDTO tradeDTO = cartService.readDTO(checkedWay);
        //将购物车到sku未选择信息过滤
        List<CartSkuVO> collect = tradeDTO.getSkuList().parallelStream().filter(i -> Boolean.TRUE.equals(i.getChecked())).collect(Collectors.toList());
        tradeDTO.setSkuList(collect);
        if (checkedWay.equals(CartTypeEnum.CART) || checkedWay.equals(CartTypeEnum.BUY_NOW) || checkedWay.equals(CartTypeEnum.VIRTUAL)) {
            //按照计划进行渲染
            for (RenderStepEnums step : defaultRender) {
                for (CartRenderStep render : cartRenderSteps) {
                    try {
                        if (render.step().equals(step)) {
                            render.render(tradeDTO);
                        }
                    } catch (Exception e) {
                        log.error("购物车{}渲染异常：", render.getClass(), e);
                    }
                }
            }
        } else {
            for (RenderStepEnums step : singleRender) {
                for (CartRenderStep render : cartRenderSteps) {
                    try {
                        if (render.step().equals(step)) {
                            render.render(tradeDTO);
                        }
                    } catch (Exception e) {
                        log.error("购物车{}渲染异常：", render.getClass(), e);
                    }
                }
            }
        }


        //购物车信息接受
        List<CartVO> cartVOList = new ArrayList<>();
        //循环购物车信息
        for (CartVO cartVO : tradeDTO.getCartList()) {
            //如果商品选中，则加入到对应购物车
            cartVO.setSkuList(cartVO.getSkuList().stream().filter(j -> Boolean.TRUE.equals(j.getChecked())).collect(Collectors.toList()));
            cartVOList.add(cartVO);
        }
        tradeDTO.setCartList(cartVOList);
        return tradeDTO;
    }

    /**
     * 创建一笔交易
     * 1.构造交易
     * 2.创建交易
     *
     * @param checkedWay 购物车类型
     * @return 交易信息
     */
    public Trade createTrade(CartTypeEnum checkedWay) {
        TradeDTO tradeDTO = this.buildTrade(checkedWay);
        return tradeService.createTrade(tradeDTO);
    }
}
