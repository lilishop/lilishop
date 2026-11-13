package cn.lili.modules.search.serviceimpl;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.search.entity.dto.GoodsSearchDTO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.mockito.ArgumentCaptor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Date;
import java.util.List;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.verify;

/**
 * 商品搜索测试共用数据及查询捕获工具，仅封装样本构造和观察操作，不注册测试用例。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
final class GoodsSearchTestFixtures {

    /** 测试数据工具类不允许实例化。 */
    private GoodsSearchTestFixtures() {
    }

    /**
     * 捕获商品搜索传递给SKU服务的查询条件。
     *
     * @param goodsSkuService 商品SKU服务
     * @return 商品搜索传递给SKU服务的查询条件
     */
    static QueryWrapper<GoodsSkuDTO> captureWrapper(GoodsSkuService goodsSkuService) {
        ArgumentCaptor<QueryWrapper<GoodsSkuDTO>> captor = ArgumentCaptor.forClass(QueryWrapper.class);
        verify(goodsSkuService).getGoodsSkuDTOByPage(any(Page.class), captor.capture());
        return captor.getValue();
    }

    /**
     * 通过反射构建商品搜索的公共查询条件。
     *
     * @param service   待测服务
     * @param searchDTO 商品搜索条件
     * @return 商品搜索公共查询条件
     */
    static QueryWrapper<GoodsSkuDTO> buildSharedWhere(GoodsSearchServiceImpl service, GoodsSearchDTO searchDTO) {
        Map<String, List<String>> selectedProps = ReflectionTestUtils.invokeMethod(service, "parseSelectedProps", searchDTO);
        return ReflectionTestUtils.invokeMethod(service, "buildSharedWhere", searchDTO, selectedProps);
    }

    /**
     * 统计指定字符串片段在文本中出现的次数。
     *
     * @param text  待检查文本
     * @param token 待统计的文本片段
     * @return 指定字符串片段在文本中出现的次数
     */
    static int countOccurrences(String text, String token) {
        int count = 0;
        int idx = 0;
        while ((idx = text.indexOf(token, idx)) >= 0) {
            count++;
            idx += token.length();
        }
        return count;
    }

    /**
     * 构造商品搜索使用的SKU实体。
     *
     * @param skuId     SKU ID
     * @param goodsId   商品ID
     * @param goodsName 商品名称
     * @return 商品搜索使用的SKU实体
     */
    static GoodsSku sku(String skuId, String goodsId, String goodsName) {
        GoodsSku sku = new GoodsSku();
        sku.setId(skuId);
        sku.setGoodsId(goodsId);
        sku.setGoodsName(goodsName);
        sku.setMarketEnable(GoodsStatusEnum.UPPER.name());
        sku.setAuthFlag(GoodsAuthEnum.PASS.name());
        sku.setPrice(99D);
        sku.setStoreId("store-1");
        sku.setStoreName("测试店铺");
        sku.setCategoryPath("cat-1");
        sku.setBrandId("brand-1");
        return sku;
    }

    /**
     * 构造包含商品参数的SKU搜索数据。
     *
     * @param skuId      SKU ID
     * @param goodsId    商品ID
     * @param goodsName  商品名称
     * @param paramName  商品参数名称
     * @param paramValue 商品参数值
     * @return 包含商品参数的SKU搜索数据
     */
    static GoodsSkuDTO skuDto(String skuId, String goodsId, String goodsName, String paramName, String paramValue) {
        GoodsSkuDTO sku = new GoodsSkuDTO();
        sku.setId(skuId);
        sku.setGoodsId(goodsId);
        sku.setGoodsName(goodsName);
        sku.setMarketEnable(GoodsStatusEnum.UPPER.name());
        sku.setAuthFlag(GoodsAuthEnum.PASS.name());
        sku.setPrice(99D);
        sku.setStoreId("store-1");
        sku.setStoreName("测试店铺");
        sku.setCategoryPath("cat-1");
        sku.setBrandId("brand-1");
        sku.setParams("""
                [{"goodsParamsItemDTOList":[{"paramName":"%s","paramValue":"%s","isIndex":1,"sort":1}]}]
                """.formatted(paramName, paramValue));
        return sku;
    }

    /**
     * 构造指定满额优惠活动的促销映射。
     *
     * @param promotionId 促销活动ID
     * @return 指定满额优惠活动的促销映射
     */
    static Map<String, Object> fullDiscountPromotionMap(String promotionId) {
        FullDiscount fullDiscount = new FullDiscount();
        fullDiscount.setId(promotionId);
        fullDiscount.setPromotionName("满减活动");
        fullDiscount.setStoreId("store-1");
        fullDiscount.setScopeType("PORTION_GOODS");
        fullDiscount.setScopeId("sku-1");
        fullDiscount.setStartTime(new Date(System.currentTimeMillis() - 1000));
        fullDiscount.setEndTime(new Date(System.currentTimeMillis() + 3600_000));
        fullDiscount.setFullMoney(100D);
        fullDiscount.setFullMinusFlag(true);
        fullDiscount.setFullMinus(10D);
        return Map.of(PromotionTypeEnum.FULL_DISCOUNT.name() + "-" + promotionId, fullDiscount);
    }
}
