package cn.lili.modules.order.cart.render.impl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.enums.RenderStepEnums;
import cn.lili.modules.order.cart.entity.vo.CartSkuVO;
import cn.lili.modules.order.cart.entity.vo.CartVO;
import cn.lili.modules.order.cart.render.CartRenderStep;
import cn.lili.modules.order.order.entity.dto.PriceDetailDTO;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 购物车渲染，将购物车中的各个商品，拆分到每个商家，形成购物车VO
 *
 * @author Chopper
 * @see CartVO
 */
@Order(5)
@Service
public class CartPriceRender implements CartRenderStep {


    /**
     * 商品分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 积分商品
     */
    @Autowired
    private PointsGoodsService pointsGoodsService;
    /**
     * 砍价商品
     */
    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;

    @Override
    public RenderStepEnums step() {
        return RenderStepEnums.CART_PRICE;
    }
    @Override
    public void render(TradeDTO tradeDTO) {
        //构造cartVO
        this.buildCart(tradeDTO);
        this.buildCartPrice(tradeDTO);
        this.buildTradePrice(tradeDTO);

    }

    /**
     * 购物车价格
     *
     * @param tradeDTO 购物车展示信息
     */
    void buildCart(TradeDTO tradeDTO) {
        for (CartVO cart : tradeDTO.getCartList()) {
            for (CartSkuVO sku : cart.getSkuList()) {
                if (Boolean.FALSE.equals(sku.getChecked())) {
                    continue;
                }
                cart.addGoodsNum(sku.getNum());
                if (cart.getStoreId().equals(sku.getStoreId()) && !cart.getSkuList().contains(sku)) {
                    cart.getSkuList().add(sku);
                }
                //写入初始价格
                sku.getPriceDetailDTO().setGoodsPrice(CurrencyUtil.mul(sku.getPurchasePrice(), sku.getNum()));
            }
        }
    }

    /**
     * 购物车价格
     *
     * @param tradeDTO 购物车展示信息
     */
    void buildCartPrice(TradeDTO tradeDTO) {
        List<CartSkuVO> cartSkuVOList = tradeDTO.getSkuList();
        //购物车列表
        List<CartVO> cartVOS = tradeDTO.getCartList();

        //key store id
        //value 商品列表
        Map<String, List<CartSkuVO>> map = new HashMap<>(2);
        for (CartSkuVO cartSkuVO : cartSkuVOList) {
            //如果存在商家id
            if (map.containsKey(cartSkuVO.getGoodsSku().getStoreId())) {
                List<CartSkuVO> list = map.get(cartSkuVO.getGoodsSku().getStoreId());
                list.add(cartSkuVO);
            } else {
                List<CartSkuVO> list = new ArrayList<>();
                list.add(cartSkuVO);
                map.put(cartSkuVO.getGoodsSku().getStoreId(), list);
            }
        }

        //计算购物车价格
        for (CartVO cart : cartVOS) {
            List<CartSkuVO> cartSkuVOS = map.get(cart.getStoreId());

            //累加价格
            List<PriceDetailDTO> priceDetailDTOS = new ArrayList<>();
            //购物车选中
            if (Boolean.TRUE.equals(cart.getChecked())) {
                for (CartSkuVO cartSkuVO : cartSkuVOS) {
                    //sku选中
                    if (Boolean.TRUE.equals(cartSkuVO.getChecked())) {
                        PriceDetailDTO priceDetailDTO = cartSkuVO.getPriceDetailDTO();
                        //平台佣金根据分类计算
                        String categoryId = cartSkuVO.getGoodsSku().getCategoryPath()
                                .substring(cartSkuVO.getGoodsSku().getCategoryPath().lastIndexOf(",") + 1);
                        if (StrUtil.isNotEmpty(categoryId)) {
                            Double commissionRate = categoryService.getById(categoryId).getCommissionRate();
                            priceDetailDTO.setCommission(commissionRate);
                        }

                        //如果积分订单 积分订单，单独操作订单结算金额和商家结算字段
                        if (tradeDTO.getCartTypeEnum().equals(CartTypeEnum.POINTS)) {
                            PointsGoodsVO pointsGoodsVO = pointsGoodsService.getPointsGoodsVOByMongo(cartSkuVO.getGoodsSku().getId());
                            priceDetailDTO.setBillPrice(pointsGoodsVO.getSettlementPrice());
                            priceDetailDTO.setSettlementPrice(pointsGoodsVO.getSettlementPrice());
                        }
                        //如果砍价订单 计算金额，单独操作订单结算金额和商家结算字段
                        else if (tradeDTO.getCartTypeEnum().equals(CartTypeEnum.KANJIA)) {
                            KanjiaActivityGoodsDTO kanjiaActivityGoodsDTO = kanjiaActivityGoodsService.getKanJiaGoodsBySku(cartSkuVO.getGoodsSku().getId());
                            priceDetailDTO.setBillPrice(kanjiaActivityGoodsDTO.getSettlementPrice());
                            priceDetailDTO.setSettlementPrice(kanjiaActivityGoodsDTO.getSettlementPrice());
                        }

                        priceDetailDTOS.add(priceDetailDTO);
                    }
                }
                cart.getPriceDetailDTO().accumulationPriceDTO(priceDetailDTOS);
            }
        }
    }


    /**
     * 初始化购物车
     *
     * @param tradeDTO 购物车展示信息
     */
    void buildTradePrice(TradeDTO tradeDTO) {
        //购物车列表
        List<CartVO> cartVOS = tradeDTO.getCartList();

        List<PriceDetailDTO> priceDetailDTOS = new ArrayList<>();
        for (CartVO cart : cartVOS) {
            priceDetailDTOS.add(cart.getPriceDetailDTO());
        }
        tradeDTO.getPriceDetailDTO().accumulationPriceDTO(priceDetailDTOS);
    }

}
