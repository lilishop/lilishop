package cn.lili.modules.search.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.service.EsGoodsSearchService;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.data.elasticsearch.client.elc.NativeQueryBuilder;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.data.elasticsearch.core.query.Query;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

/**
 * all-in-one 精简模式不启用 Elasticsearch，搜索入口返回空结果以保证业务主流程可启动。
 */
@Service
@ConditionalOnProperty(value = "lili.data.elasticsearch.enabled", havingValue = "false")
public class EsGoodsSearchServiceNoop implements EsGoodsSearchService {

    @Override
    public SearchPage<EsGoodsIndex> searchGoods(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        return null;
    }

    @Override
    public <T> SearchPage<T> searchGoods(Query searchQuery, Class<T> clazz) {
        return null;
    }

    @Override
    public Page<EsGoodsIndex> searchGoodsByPage(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        int pageNumber = pageVo == null ? 1 : pageVo.getPageNumber();
        int pageSize = pageVo == null ? 10 : pageVo.getPageSize();
        return new Page<>(pageNumber, pageSize);
    }

    @Override
    public EsGoodsRelatedInfo getSelector(EsGoodsSearchDTO goodsSearch, PageVO pageVo) {
        EsGoodsRelatedInfo relatedInfo = new EsGoodsRelatedInfo();
        relatedInfo.setCategories(Collections.emptyList());
        relatedInfo.setBrands(Collections.emptyList());
        relatedInfo.setParamOptions(Collections.emptyList());
        return relatedInfo;
    }

    @Override
    public List<EsGoodsIndex> getEsGoodsBySkuIds(List<String> skuIds, PageVO pageVo) {
        return Collections.emptyList();
    }

    @Override
    public EsGoodsIndex getEsGoodsById(String id) {
        return null;
    }

    @Override
    public NativeQueryBuilder createSearchQueryBuilder(EsGoodsSearchDTO searchDTO, PageVO pageVo) {
        return new NativeQueryBuilder();
    }
}
