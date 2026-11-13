package cn.lili.modules.promotion.support;

import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.mapper.SeckillApplyMapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.stereotype.Component;

/**
 * 秒杀申请销量回写能力，供促销库存更新和秒杀申请服务共用，避免两个业务服务相互依赖。
 * 沿用调用方事务，销量按传入累计值覆盖，不执行增量累加，也不创建缺失的申请记录。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class SeckillApplySalesSupport {

    private final SeckillApplyMapper mapper;

    /**
     * 注入秒杀申请存储，不依赖促销商品或秒杀活动服务。
     *
     * @param mapper 秒杀申请存储
     */
    public SeckillApplySalesSupport(SeckillApplyMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * 申请存在时回写销量，沿用秒杀申请查询条件，不额外限制审核状态或活动时间。
     * 查询和更新均参与调用方事务；记录在查询后被删除时，更新不新增记录。
     *
     * @param seckillId 秒杀活动主键
     * @param skuId 商品SKU主键
     * @param saleNum 当前累计销量，单位件
     */
    public void updateSaleNumIfPresent(String seckillId, String skuId, Integer saleNum) {
        SeckillSearchParams searchParams = new SeckillSearchParams();
        searchParams.setSeckillId(seckillId);
        searchParams.setSkuId(skuId);
        if (mapper.selectOne(searchParams.queryWrapper(), false) != null) {
            updateSaleNum(seckillId, skuId, saleNum);
        }
    }

    /**
     * 按活动和SKU覆盖所有匹配申请的累计销量，匹配不到记录时不做写入。
     *
     * @param seckillId 秒杀活动主键
     * @param skuId 商品SKU主键
     * @param saleNum 当前累计销量，单位件
     */
    public void updateSaleNum(String seckillId, String skuId, Integer saleNum) {
        LambdaUpdateWrapper<SeckillApply> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(SeckillApply::getSeckillId, seckillId).eq(SeckillApply::getSkuId, skuId);
        updateWrapper.set(SeckillApply::getSalesNum, saleNum);
        mapper.update(null, updateWrapper);
    }
}
