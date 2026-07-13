package cn.lili.modules.search.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.entity.dos.GoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * 商品搜索业务层（MySQL 实现）
 */
public interface GoodsSearchService {

    /**
     * 商品搜索分页
     */
    Page<GoodsIndex> searchGoodsByPage(GoodsSearchDTO searchDTO, PageVO pageVo);

    /**
     * 获取筛选器
     */
    GoodsRelatedInfo getSelector(GoodsSearchDTO goodsSearch, PageVO pageVo);

    /**
     * 根据 SkuID 列表获取商品
     */
    List<GoodsIndex> getGoodsBySkuIds(List<String> skuIds, PageVO pageVo);

    /**
     * 根据 id 获取商品
     */
    GoodsIndex getGoodsById(String id);
}
