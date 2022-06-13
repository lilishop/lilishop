package cn.lili.test.elasticsearch;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.service.PromotionService;
import cn.lili.modules.search.entity.dos.EsGoodsAttribute;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.entity.dos.EsGoodsRelatedInfo;
import cn.lili.modules.search.entity.dto.EsGoodsSearchDTO;
import cn.lili.modules.search.repository.EsGoodsIndexRepository;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.modules.search.service.EsGoodsSearchService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.elasticsearch.core.SearchPage;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.ArrayList;
import java.util.List;

/**
 * @author paulG
 * @since 2020/10/14
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
class EsTest {

    @Autowired
    private EsGoodsIndexService esGoodsIndexService;

    @Autowired
    private EsGoodsIndexRepository goodsIndexRepository;

    @Autowired
    private EsGoodsSearchService goodsSearchService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private GoodsService goodsService;

    @Autowired
    private Cache cache;

    @Autowired
    private PromotionService promotionService;


    public static void main(String[] args) {
//        PolicyFactory policy = Sanitizers.FORMATTING.and(Sanitizers.LINKS);
//        String safeHTML = policy.sanitize("+ADw-script+AD4-alert(document.cookie)+ADw-/script+AD4-");
//        System.out.println(safeHTML);
//        System.out.println(Sanitizers.FORMATTING.and(Sanitizers.FORMATTING).sanitize("+ADw-script+AD4-alert(document.cookie)+ADw-/script+AD4-"));
//        System.out.println(HtmlUtil.unescape(safeHTML));
//        System.out.println(HtmlUtil.filter("+ADw-script+AD4-alert(document.cookie)+ADw-/script+AD4-"));
//        Date dt1 = new Date(2021, 12, 10);
//        Date dt2 = new Date(2021, 12, 14);
        for (int i = 0; i < 1000; i++) {

            Goods goods = new Goods();
            goods.setGoodsName("测试商品" + i);
//            goods.setAuthFlag();
        }

//

    }

    @Test
    void cleanInvalidPromotion() {
        this.esGoodsIndexService.cleanInvalidPromotion();
        Assertions.assertTrue(true);
    }

    @Test
    void searchGoods() {
        EsGoodsSearchDTO goodsSearchDTO = new EsGoodsSearchDTO();
//       goodsSearchDTO.setKeyword("黄");
//        goodsSearchDTO.setProp("IETF_HTTP/3");
//       goodsSearchDTO.setPrice("100_20000");
//       goodsSearchDTO.setStoreCatId(1L);
//       goodsSearchDTO.setBrandId(123L);
//       goodsSearchDTO.setCategoryId(2L);
//       goodsSearchDTO.setNameIds(Arrays.asList("1344113311566553088", "1344113367694729216"));
        PageVO pageVo = new PageVO();
        pageVo.setPageNumber(0);
        pageVo.setPageSize(100);
        pageVo.setOrder("desc");
        pageVo.setNotConvert(true);
        SearchPage<EsGoodsIndex> esGoodsIndices = goodsSearchService.searchGoods(goodsSearchDTO, pageVo);
        Assertions.assertNotNull(esGoodsIndices);
        esGoodsIndices.getContent().forEach(System.out::println);
//       esGoodsIndices.getContent().forEach(i -> {
//           if (i.getPromotionMap() != null){
//               String s = i.getPromotionMap().keySet().parallelStream().filter(j -> j.contains(PromotionTypeEnum.FULL_DISCOUNT.name())).findFirst().orElse(null);
//               if (s != null) {
//                   FullDiscount basePromotion = (FullDiscount) i.getPromotionMap().get(s);
//                   System.out.println(basePromotion);
//               }
//           }
//       });

    }

    @Test
    void aggregationSearch() {
        EsGoodsSearchDTO goodsSearchDTO = new EsGoodsSearchDTO();
        //goodsSearchDTO.setKeyword("电脑");
        //goodsSearchDTO.setProp("颜色_故宫文创@版本_小新Pro13s");
//       goodsSearchDTO.setCategoryId("2");
//       goodsSearchDTO.setPrice("100_20000");
        PageVO pageVo = new PageVO();
        pageVo.setPageNumber(0);
        pageVo.setPageSize(10);
        pageVo.setOrder("desc");
        EsGoodsRelatedInfo selector = goodsSearchService.getSelector(goodsSearchDTO, pageVo);
        Assertions.assertNotNull(selector);
        System.out.println(JSONUtil.toJsonStr(selector));

    }

    @Test
    void init() {
        LambdaQueryWrapper<GoodsSku> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(GoodsSku::getAuthFlag, GoodsAuthEnum.PASS.name());
        queryWrapper.eq(GoodsSku::getMarketEnable, GoodsStatusEnum.UPPER.name());
        List<GoodsSku> list = goodsSkuService.list(queryWrapper);
        List<EsGoodsIndex> esGoodsIndices = new ArrayList<>();
        for (GoodsSku goodsSku : list) {
            EsGoodsIndex index = new EsGoodsIndex(goodsSku);
            esGoodsIndices.add(index);
            cache.put(GoodsSkuService.getStockCacheKey(goodsSku.getId()), goodsSku.getQuantity());
        }
        esGoodsIndexService.initIndex(esGoodsIndices, true);
        Assertions.assertTrue(true);
    }

    @Test
    void addIndex() {
        List<EsGoodsAttribute> esGoodsAttributeList = new ArrayList<>();
        EsGoodsAttribute attribute = new EsGoodsAttribute();
        attribute.setType(0);
        attribute.setName("颜色");
        attribute.setValue("16.1英寸 6核R5 16G 512G 高色域");
        esGoodsAttributeList.add(attribute);
        attribute = new EsGoodsAttribute();
        attribute.setType(0);
        attribute.setName("版本");
        attribute.setValue("RedmiBook 18英寸 深空灰");
        esGoodsAttributeList.add(attribute);
        EsGoodsIndex goodsIndex = initGoodsIndexData("122", "0|2", "140", "142", "A142", "RedmiBook 18 锐龙版 超轻薄全面屏(6核R5-4500U 16G 512G 100% sRGB高色域)灰 手提 笔记本电脑 小米 红米 ", "131", "小米自营旗舰店", 10000D);
        goodsIndex.setAttrList(esGoodsAttributeList);

        //GoodsSku goodsSkuByIdFromCache = goodsSkuService.getGoodsSkuByIdFromCache("121");
        //EsGoodsIndex goodsIndex = new EsGoodsIndex(goodsSkuByIdFromCache);


        esGoodsIndexService.addIndex(goodsIndex);

        Assertions.assertTrue(true);
    }

    @Test
    void searchAll() {
        Iterable<EsGoodsIndex> all = goodsIndexRepository.findAll();
        Assertions.assertNotNull(all);
        all.forEach(System.out::println);
    }

    @Test
    void updateIndex() {
        EsGoodsIndex byId = esGoodsIndexService.findById("121");
        esGoodsIndexService.updateIndex(byId);
        Assertions.assertTrue(true);
    }

    @Test
    void deleteIndex() {
        esGoodsIndexService.deleteIndex(null);
        Assertions.assertTrue(true);
    }

    @Test
    void cleanPromotion() {
        esGoodsIndexService.cleanInvalidPromotion();
        Assertions.assertTrue(true);
    }


    private EsGoodsIndex initGoodsIndexData(String brandId, String categoryPath, String goodsId, String id, String sn, String goodsName, String storeId, String storeName, Double price) {
        EsGoodsIndex goodsIndex = new EsGoodsIndex();
        goodsIndex.setBuyCount(99);
        goodsIndex.setCommentNum(99);
        goodsIndex.setGrade(100D);
        goodsIndex.setHighPraiseNum(100);
        goodsIndex.setIntro("I'd like a cup of tea, please");
        goodsIndex.setAuthFlag("1");
        goodsIndex.setMarketEnable("1");
        goodsIndex.setMobileIntro("I want something cold to drink");
        goodsIndex.setPoint(0);
        goodsIndex.setSelfOperated(true);
        goodsIndex.setThumbnail("picture");
        goodsIndex.setStoreCategoryPath("1");

        goodsIndex.setId(id);
        goodsIndex.setBrandId(brandId);
        goodsIndex.setGoodsId(goodsId);
        goodsIndex.setCategoryPath(categoryPath);
        goodsIndex.setGoodsName(goodsName);
        goodsIndex.setPrice(price);
        goodsIndex.setSn(sn);
        goodsIndex.setStoreId(storeId);
        goodsIndex.setStoreName(storeName);
        return goodsIndex;
    }


}
