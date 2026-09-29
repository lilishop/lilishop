package cn.lili.modules.member.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.member.entity.dto.FootPrintQueryParams;
import cn.lili.modules.member.mapper.FootprintMapper;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 客户浏览历史业务层实现
 *
 * @author Chopper
 * @since 2020/11/18 10:46 上午
 */
@Service
public class FootprintServiceImpl extends ServiceImpl<FootprintMapper, FootPrint> implements FootprintService {


    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public FootPrint saveFootprint(FootPrint footPrint) {
        LambdaQueryWrapper<FootPrint> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(FootPrint::getMemberId, footPrint.getMemberId());
        queryWrapper.eq(FootPrint::getGoodsId, footPrint.getGoodsId());
        //如果已存在某商品记录，则更新其修改时间
        //如果不存在则添加记录
        //为了保证足迹的排序,将原本足迹删除后重新添加
        List<FootPrint> oldPrints = list(queryWrapper);
        if (oldPrints != null && !oldPrints.isEmpty()) {
            FootPrint oldPrint = oldPrints.get(0);
            this.removeById(oldPrint.getId());
        }
        footPrint.setCreateTime(new Date());
        this.save(footPrint);
        //删除超过100条后的记录
        this.baseMapper.deleteLastFootPrint(footPrint.getMemberId());
        return footPrint;
    }

    @Override
    public boolean clean() {
        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, UserContext.getCurrentUser().getId());
        return this.remove(lambdaQueryWrapper);
    }

    @Override
    public boolean deleteByIds(List<String> ids) {
        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, UserContext.getCurrentUser().getId());
        lambdaQueryWrapper.in(FootPrint::getGoodsId, ids);
        return this.remove(lambdaQueryWrapper);
    }

    @Override
    public IPage<GoodsIndex> footPrintPage(FootPrintQueryParams params) {
        int pageNumber = params.getPageNumber() == null || params.getPageNumber() < 1 ? 1 : params.getPageNumber();
        int pageSize = params.getPageSize() == null || params.getPageSize() < 1 ? 20 : params.getPageSize();
        List<FootPrint> footprints = this.list(params.queryWrapper());
        List<GoodsIndex> goodsList = toVisibleGoods(footprints);

        int from = (pageNumber - 1) * pageSize;
        List<GoodsIndex> pageRecords = from >= goodsList.size()
                ? new ArrayList<>()
                : new ArrayList<>(goodsList.subList(from, Math.min(goodsList.size(), from + pageSize)));

        Page<GoodsIndex> goodsIndexIPage = new Page<>(pageNumber, pageSize, goodsList.size());
        goodsIndexIPage.setRecords(pageRecords);
        return goodsIndexIPage;
    }

    /**
     * 只保留还能查到商品的足迹。商品已删除或缓存缺记录时不再占分页名额。
     */
    private List<GoodsIndex> toVisibleGoods(List<FootPrint> footprints) {
        List<GoodsIndex> goodsList = new ArrayList<>();
        if (footprints == null || footprints.isEmpty()) {
            return goodsList;
        }
        List<String> skuIds = new ArrayList<>();
        for (FootPrint footPrint : footprints) {
            if (footPrint.getSkuId() != null) {
                skuIds.add(footPrint.getSkuId());
            }
        }
        Map<String, GoodsSku> skuMap = new HashMap<>();
        List<GoodsSku> cachedSkus = goodsSkuService.getGoodsSkuByIdFromCache(skuIds);
        if (cachedSkus != null) {
            for (GoodsSku sku : cachedSkus) {
                if (sku != null && sku.getId() != null) {
                    skuMap.put(sku.getId(), sku);
                }
            }
        }
        for (String skuId : skuIds) {
            if (skuMap.containsKey(skuId)) {
                continue;
            }
            GoodsSku sku = goodsSkuService.getGoodsSkuByIdFromCache(skuId);
            if (sku != null && sku.getId() != null) {
                skuMap.put(sku.getId(), sku);
            }
        }
        for (FootPrint footPrint : footprints) {
            GoodsSku sku = skuMap.get(footPrint.getSkuId());
            if (sku == null) {
                continue;
            }
            goodsList.add(new GoodsIndex(sku, footPrint.getCreateTime()));
        }
        return goodsList;
    }

    @Override
    public long getFootprintNum() {
        LambdaQueryWrapper<FootPrint> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(FootPrint::getMemberId, Objects.requireNonNull(UserContext.getCurrentUser()).getId());
        lambdaQueryWrapper.eq(FootPrint::getDeleteFlag, false);
        return this.count(lambdaQueryWrapper);
    }
}