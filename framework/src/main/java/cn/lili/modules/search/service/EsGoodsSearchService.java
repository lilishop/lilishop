package cn.lili.modules.search.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.data.elasticsearch.core.query.NativeSearchQueryBuilder;
import org.springframework.data.elasticsearch.core.query.Query;

import java.util.List;

/**
 * ES商品搜索业务层
 *
 * @author paulG
 * @since 2020/10/15
 **/
public interface EsGoodsSearchService {

    /**
     * 商品搜索
     *
     * @param searchDTO 搜索参数
     * @param pageVo    分页参数
     * @return 搜索结果
     */
    SearchPage<EsGoodsIndex> searchGoods(EsGoodsSearchDTO searchDTO, PageVO pageVo);

    /**
     * 搜索商品
     *
     * @param searchQuery 搜索条件
     * @param clazz       搜索结果类
     * @param <T>         泛型
     * @return 搜索结果
     */
    <T> SearchPage<T> searchGoods(Query searchQuery, Class<T> clazz);

    /**
     * 商品搜索
     *
     * @param searchDTO 搜索参数
     * @param pageVo    分页参数
     * @return 搜索结果
     */
    Page<EsGoodsIndex> searchGoodsByPage(EsGoodsSearchDTO searchDTO, PageVO pageVo);

    /**
     * 获取筛选器
     *
     * @param goodsSearch 搜索条件
     * @param pageVo      分页参数
     * @return ES商品关联
     */
    EsGoodsRelatedInfo getSelector(EsGoodsSearchDTO goodsSearch, PageVO pageVo);

    /**
     * 根据SkuID列表获取ES商品
     *
     * @param skuIds SkuId列表
     * @return ES商品列表
     */
    List<EsGoodsIndex> getEsGoodsBySkuIds(List<String> skuIds, PageVO pageVo);

    /**
     * 根据id获取商品索引
     *
     * @param id 商品skuId
     * @return 商品索引
     */
    EsGoodsIndex getEsGoodsById(String id);

    /**
     * 创建搜索条件
     *
     * @param searchDTO 搜索参数
     * @param pageVo    分页参数
     * @return 搜索条件
     */
    NativeSearchQueryBuilder createSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo);
}
