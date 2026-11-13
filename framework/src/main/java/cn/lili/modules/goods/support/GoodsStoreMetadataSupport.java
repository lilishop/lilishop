package cn.lili.modules.goods.support;

import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.store.entity.dos.Store;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * 商品上的店铺基础信息同步，供商品和店铺详情服务共用，避免店铺详情回调完整商品服务。
 * 只维护原有店铺名称和自营标志字段，不额外同步SKU、清理缓存或发送消息。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class GoodsStoreMetadataSupport {

    private final GoodsMapper mapper;

    /**
     * 注入商品存储，不依赖商品或店铺业务服务。
     *
     * @param mapper 商品存储
     */
    public GoodsStoreMetadataSupport(GoodsMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * 按店铺主键同步未逻辑删除商品的店铺名称和自营标志，不额外限定商品上下架状态。
     * 沿用原有事务规则，加入调用方事务，无事务时开启事务。
     *
     * @param store 提供店铺主键、店铺名称和自营标志的店铺实体
     */
    @Transactional(rollbackFor = Exception.class)
    public void updateStoreDetail(Store store) {
        mapper.update(null, new LambdaUpdateWrapper<Goods>()
                .eq(Goods::getStoreId, store.getId())
                .set(Goods::getStoreName, store.getStoreName())
                .set(Goods::getSelfOperated, store.getSelfOperated()));
    }
}
