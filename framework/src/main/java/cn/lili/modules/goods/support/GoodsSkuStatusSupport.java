package cn.lili.modules.goods.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 店铺SKU状态同步，供商品上下架与SKU服务共用，不依赖完整SKU或店铺业务服务。
 * 状态更新后沿用原规则清理SKU详情及库存缓存，不额外清理SPU缓存或发送事件。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class GoodsSkuStatusSupport {
    private final GoodsSkuMapper mapper;
    private final Cache cache;

    /**
     * 注入SKU存储和缓存。
     *
     * @param mapper SKU存储
     * @param cache SKU详情和库存缓存
     */
    public GoodsSkuStatusSupport(GoodsSkuMapper mapper, Cache cache) {
        this.mapper = mapper;
        this.cache = cache;
    }

    /**
     * 按指定店铺更新未逻辑删除SKU，只有非空状态参数参与更新；不额外使用当前店铺限定范围。
     * 数据库更新沿用调用方事务，无事务时开启事务；缓存立即删除，不随数据库回滚恢复。
     *
     * @param storeId 店铺主键
     * @param marketEnable 上下架状态，非空时更新，沿用UPPER或DOWN编码
     * @param authFlag 审核状态，非空时更新，沿用GoodsAuthEnum编码
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateByStoreId(String storeId, String marketEnable, String authFlag) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getStoreId, storeId);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(marketEnable), GoodsSku::getMarketEnable, marketEnable);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(authFlag), GoodsSku::getAuthFlag, authFlag);
        mapper.update(null, updateWrapper);
        List<GoodsSku> list = mapper.selectList(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getStoreId, storeId));
        for (GoodsSku goodsSku : list) {
            cache.remove(GoodsSkuService.getCacheKeys(goodsSku.getId()));
            cache.remove(GoodsSkuService.getStockCacheKey(goodsSku.getId()));
        }
    }
}
