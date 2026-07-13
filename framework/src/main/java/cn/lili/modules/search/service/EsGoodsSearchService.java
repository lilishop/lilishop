package cn.lili.modules.search.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

import java.util.List;

/**
 * 商品搜索业务层（MySQL 实现）
 */
public interface EsGoodsSearchService {

    /**
     * 商品搜索分页
     */
    Page<EsGoodsIndex> searchGoodsByPage(EsGoodsSearchDTO searchDTO, PageVO pageVo);

    /**
     * 获取筛选器
     */
    EsGoodsRelatedInfo getSelector(EsGoodsSearchDTO goodsSearch, PageVO pageVo);

    /**
     * 根据 SkuID 列表获取商品
     */
    List<EsGoodsIndex> getEsGoodsBySkuIds(List<String> skuIds, PageVO pageVo);

    /**
     * 根据 id 获取商品
     */
    EsGoodsIndex getEsGoodsById(String id);
}
