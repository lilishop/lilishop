package cn.lili.event.impl;

import cn.lili.common.cache.Cache;
import cn.lili.event.OrderStatusChangeEvent;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.core.script.DefaultRedisScript;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 库存扣减，他表示了订单状态是否出库成功
 *
 * @author Chopper
 * @date 2020-07-03 11:20
 */
@Service
public class StockUpdateExecute implements OrderStatusChangeEvent {

    //Redis
    @Autowired
    private StringRedisTemplate stringRedisTemplate;
    @Autowired
    private DefaultRedisScript<Boolean> quantityScript;
    //订单
    @Autowired
    private OrderService orderService;
    //规格商品
    @Autowired
    private GoodsSkuService goodsSkuService;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //缓存
    @Autowired
    private Cache cache;

    @Override
    public void orderChange(OrderMessage orderMessage) {

        switch (orderMessage.getNewStatus()) {
            case PAID: {

                OrderDetailVO order = orderService.queryDetail(orderMessage.getOrderSn());
                //库存key 和 扣减数量
                List<String> keys = new ArrayList<>();
                List<String> values = new ArrayList<>();
                for (OrderItem orderItem : order.getOrderItems()) {
                    keys.add(GoodsSkuService.getStockCacheKey(orderItem.getSkuId()));
                    int i = -orderItem.getNum();
                    values.add(Integer.toString(i));
                    setPromotionStock(keys, values, orderItem);
                }
                //库存扣除结果
                Boolean skuResult = stringRedisTemplate.execute(quantityScript, keys, values.toArray());
                //如果库存扣减都成功，则记录成交订单
                if (Boolean.TRUE.equals(skuResult)) {
                    //库存确认之后对结构处理
                    orderService.afterOrderConfirm(orderMessage.getOrderSn());
                    //成功之后，同步库存
                    synchroDB(order);
                } else {
                    //失败之后取消订单
                    this.errorOrder(orderMessage.getOrderSn());
                }
                break;
            }
            case CANCELLED: {

                OrderDetailVO order = orderService.queryDetail(orderMessage.getOrderSn());
                if (order.getOrder().getPayStatus().equals(PayStatusEnum.PAID.name())) {
                    for (OrderItem orderItem : order.getOrderItems()) {
                        if (PromotionTypeEnum.haveStock(orderItem.getPromotionType())) {
                            PromotionTypeEnum promotionTypeEnum = PromotionTypeEnum.valueOf(orderItem.getPromotionType());
                            Integer goodsPromotionOriginStock = promotionGoodsService.getPromotionGoodsStock(promotionTypeEnum, orderItem.getPromotionId(), orderItem.getSkuId());
                            int goodsPromotionStock = goodsPromotionOriginStock + orderItem.getNum();
                            String promotionGoodsStockCacheKey = PromotionGoodsService.getPromotionGoodsStockCacheKey(promotionTypeEnum, orderItem.getPromotionId(), orderItem.getSkuId());
                            stringRedisTemplate.opsForValue().set(promotionGoodsStockCacheKey, Integer.toString(goodsPromotionStock));
                        }
                        String stockCacheKey = GoodsSkuService.getStockCacheKey(orderItem.getSkuId());
                        Integer goodsOriginStock = goodsSkuService.getStock(orderItem.getSkuId());
                        int goodsStock = goodsOriginStock + orderItem.getNum();
                        stringRedisTemplate.opsForValue().set(stockCacheKey, Integer.toString(goodsStock));
                    }
                }
                break;
            }
            default:
                break;
        }
    }

    /**
     * 订单出库失败
     *
     * @param orderSn 失败入库订单信息
     */
    private void errorOrder(String orderSn) {
        orderService.systemCancel(orderSn, "库存不足，出库失败");
    }


    /**
     * 写入需要更改促销库存的商品
     *
     * @param keys   缓存key值
     * @param values 缓存value值
     * @param sku    购物车信息
     */
    private void setPromotionStock(List<String> keys, List<String> values, OrderItem sku) {
        if (sku.getPromotionType() != null) {
            //如果此促销有库存概念，则计入
            if (!PromotionTypeEnum.haveStock(sku.getPromotionType())) {
                return;
            }
            PromotionTypeEnum promotionTypeEnum = PromotionTypeEnum.valueOf(sku.getPromotionType());
            keys.add(PromotionGoodsService.getPromotionGoodsStockCacheKey(promotionTypeEnum, sku.getPromotionId(), sku.getSkuId()));
            int i = -sku.getNum();
            values.add(Integer.toString(i));
        }
    }

    /**
     * 写入需要更改促销库存的商品
     *
     * @param order 订单
     */
    private void synchroDB(OrderDetailVO order) {

        //sku商品
        List<GoodsSku> goodsSkus = new ArrayList<>();
        //促销商品
        List<PromotionGoods> promotionGoods = new ArrayList<>();
        //sku库存key 集合
        List<String> skuKeys = new ArrayList<>();
        //促销库存key 集合
        List<String> promotionKey = new ArrayList<>();

        // 循环订单
        for (OrderItem orderItem : order.getOrderItems()) {
            skuKeys.add(GoodsSkuService.getStockCacheKey(orderItem.getSkuId()));
            GoodsSku goodsSku = new GoodsSku();
            goodsSku.setId(orderItem.getSkuId());
            //如果有促销信息
            if (null != orderItem.getPromotionType() && null != orderItem.getPromotionId()) {
                //如果促销有库存信息
                if (PromotionTypeEnum.haveStock(orderItem.getPromotionType())) {
                    PromotionTypeEnum promotionTypeEnum = PromotionTypeEnum.valueOf(orderItem.getPromotionType());
                    PromotionGoods pGoods = promotionGoodsService.getPromotionGoods(promotionTypeEnum, orderItem.getPromotionId(), orderItem.getSkuId());
                    promotionKey.add(
                            PromotionGoodsService.getPromotionGoodsStockCacheKey(
                                    promotionTypeEnum,
                                    orderItem.getPromotionId(), orderItem.getSkuId())
                    );
                    promotionGoods.add(pGoods);
                }
            }
            goodsSkus.add(goodsSku);
        }

        List skuStocks = cache.multiGet(skuKeys);
        //循环写入商品库存
        for (int i = 0; i < skuStocks.size(); i++) {
            goodsSkus.get(i).setQuantity(Integer.parseInt(skuStocks.get(i).toString()));
        }
        //批量修改商品库存
        goodsSkuService.updateBatchById(goodsSkus);

        //促销库存处理
        if (!promotionKey.isEmpty()) {
            List promotionStocks = cache.multiGet(promotionKey);
            for (int i = 0; i < promotionKey.size(); i++) {
                promotionGoods.get(i).setQuantity(Integer.parseInt(promotionStocks.get(i).toString()));
                Integer num = promotionGoods.get(i).getNum();
                promotionGoods.get(i).setNum((num != null ? num : 0) + order.getOrder().getGoodsNum());
            }
            promotionGoodsService.updateBatchById(promotionGoods);
        }
        goodsSkuService.updateGoodsStuck(goodsSkus);

    }

}
