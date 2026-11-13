package cn.lili.modules.goods.support;

import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.CardKeyStatusEnum;
import cn.lili.modules.goods.mapper.CardKeyMapper;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * 卡密池删除及库存同步能力，不依赖完整卡密履约、订单或商品服务。
 * 仅软删未使用和已作废卡密，已出售卡密保留；卡池、SKU及商品库存写入参与同一事务。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class CardKeyPoolSupport {
    private final CardKeyMapper cards;
    private final GoodsSkuMapper skus;
    private final GoodsSkuStockManager stocks;
    private final GoodsStockAggregateSupport aggregates;

    /**
     * 注入卡密存储及库存组件，禁止通过完整履约服务回调商品服务。
     *
     * @param cards 卡密存储
     * @param skus SKU存储
     * @param stocks SKU库存管理组件
     * @param aggregates 商品库存汇总组件
     */
    public CardKeyPoolSupport(CardKeyMapper cards, GoodsSkuMapper skus, GoodsSkuStockManager stocks,
                              GoodsStockAggregateSupport aggregates) {
        this.cards = cards;
        this.skus = skus;
        this.stocks = stocks;
        this.aggregates = aggregates;
    }

    /**
     * 统计未删除且未使用的卡密，作为电子卡券唯一可售库存依据。
     *
     * @param skuId SKU主键
     * @return 可售卡密数量，超出整数范围时抛出算术异常
     */
    public int countUnusedPoolStock(String skuId) {
        return Math.toIntExact(cards.selectCount(new LambdaQueryWrapper<CardKey>()
                .eq(CardKey::getSkuId, skuId).eq(CardKey::getStatus, CardKeyStatusEnum.UNUSED.name())
                .eq(CardKey::getDeleteFlag, false)));
    }

    /**
     * 按卡池真实数量同步SKU和商品库存，不允许沿用人工库存数量；SKU不存在时跳过。
     *
     * @param skuId SKU主键
     */
    @Transactional(rollbackFor = Exception.class)
    public void syncSkuStock(String skuId) {
        GoodsSku sku = skus.selectById(skuId);
        if (sku == null) {
            return;
        }
        stocks.updateStockFromCardKeySync(skuId, countUnusedPoolStock(skuId));
        aggregates.updateStock(sku.getGoodsId());
    }

    /**
     * 删除SKU关联的未使用或已作废卡密后始终重算库存，重复调用不影响已出售卡密。
     *
     * @param skuId 被删除SKU的主键
     */
    @Transactional(rollbackFor = Exception.class)
    public void softDeletePoolOnSkuRemove(String skuId) {
        cards.update(null, new LambdaUpdateWrapper<CardKey>().eq(CardKey::getSkuId, skuId)
                .inSql(CardKey::getStatus, "'UNUSED', 'VOIDED'").eq(CardKey::getDeleteFlag, false)
                .set(CardKey::getDeleteFlag, true));
        syncSkuStock(skuId);
    }
}
