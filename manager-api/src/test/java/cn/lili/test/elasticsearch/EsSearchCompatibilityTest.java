package cn.lili.test.elasticsearch;

import cn.lili.common.vo.PageVO;
import cn.lili.elasticsearch.EsSuffix;
import cn.lili.elasticsearch.config.ElasticsearchProperties;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.api.Assumptions;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.IndexOperations;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.data.elasticsearch.core.mapping.IndexCoordinates;
import org.springframework.test.context.junit.jupiter.SpringExtension;

/**
 * Elasticsearch 7.x + elasticsearch-java 9.x 搜索功能回归测试。
 * 需要可访问的 ES 集群及已初始化的商品索引；不可用时自动跳过。
 * 默认不执行，需设置环境变量 {@code LILISHOP_ES_REGRESSION=true} 且本地 Redis/MySQL/ES 可用。
 */
@EnabledIfEnvironmentVariable(named = "LILISHOP_ES_REGRESSION", matches = "true")
@Tag("es")
@ExtendWith(SpringExtension.class)
@SpringBootTest
class EsSearchCompatibilityTest {

    @Autowired
    private ElasticsearchOperations elasticsearchOperations;

    @Autowired
    private ElasticsearchProperties elasticsearchProperties;

    @Autowired
    private EsGoodsSearchService goodsSearchService;

    @Autowired
    private EsGoodsIndexService esGoodsIndexService;

    private IndexCoordinates goodsIndexCoordinates;

    @BeforeEach
    void assumeElasticsearchAvailable() {
        goodsIndexCoordinates = IndexCoordinates.of(
                elasticsearchProperties.getIndexPrefix() + "_" + EsSuffix.GOODS_INDEX_NAME);
        try {
            IndexOperations indexOperations = elasticsearchOperations.indexOps(goodsIndexCoordinates);
            Assumptions.assumeTrue(indexOperations.exists(), "goods index does not exist: " + goodsIndexCoordinates.getIndexName());
        } catch (Exception ex) {
            Assumptions.assumeTrue(false, "elasticsearch cluster unavailable: " + ex.getMessage());
        }
    }

    @Test
    void goodsIndexShouldExist() {
        Assumptions.assumeTrue(elasticsearchOperations.indexOps(goodsIndexCoordinates).exists());
    }

    @Test
    void searchGoodsShouldReturnPage() {
        EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(0);
        pageVO.setPageSize(10);
        pageVO.setOrder("desc");
        pageVO.setNotConvert(true);

        SearchPage<EsGoodsIndex> searchPage = goodsSearchService.searchGoods(searchDTO, pageVO);

        Assumptions.assumeFalse(searchPage.isEmpty(), "goods index is empty, skip search assertion");
        org.junit.jupiter.api.Assertions.assertNotNull(searchPage.getContent());
    }

    @Test
    void selectorAggregationShouldWork() {
        EsGoodsSearchDTO searchDTO = new EsGoodsSearchDTO();
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(0);
        pageVO.setPageSize(10);
        pageVO.setOrder("desc");

        EsGoodsRelatedInfo selector = goodsSearchService.getSelector(searchDTO, pageVO);
        org.junit.jupiter.api.Assertions.assertNotNull(selector);
    }

    @Test
    void findByIdShouldRoundTripWhenIndexHasDocuments() {
        SearchPage<EsGoodsIndex> searchPage = goodsSearchService.searchGoods(new EsGoodsSearchDTO(), pageVO());
        Assumptions.assumeFalse(searchPage.isEmpty(), "goods index is empty, skip findById test");

        String documentId = searchPage.getContent().get(0).getId();
        EsGoodsIndex document = esGoodsIndexService.findById(documentId);
        org.junit.jupiter.api.Assertions.assertNotNull(document);
        org.junit.jupiter.api.Assertions.assertEquals(documentId, document.getId());
    }

    private PageVO pageVO() {
        PageVO pageVO = new PageVO();
        pageVO.setPageNumber(0);
        pageVO.setPageSize(1);
        pageVO.setOrder("desc");
        pageVO.setNotConvert(true);
        return pageVO;
    }
}
