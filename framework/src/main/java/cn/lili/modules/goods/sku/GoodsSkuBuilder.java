package cn.lili.modules.goods.sku;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.lang.Assert;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * @author paulG
 * @since 2022/5/20
 **/
@Component
public class GoodsSkuBuilder {


    private static final String IMAGES_KEY = "images";


    /**
     * 构建商品sku
     *
     * @param goods   商品
     * @param skuInfo sku信息列表
     * @return 商品sku
     */
    public static GoodsSku build(Goods goods, Map<String, Object> skuInfo) {
        GoodsSku goodsSku = new GoodsSku(goods);
        builderSingle(goodsSku, skuInfo);
        return goodsSku;
    }

    /**
     * 批量构建商品sku
     *
     * @param goods 商品
     * @return 商品sku
     */
    public static List<GoodsSku> buildBatch(Goods goods, List<Map<String, Object>> skuList) {
        Assert.notEmpty(skuList, "skuList不能为空");
        Assert.notNull(goods, "goods不能为空");
        List<GoodsSku> goodsSkus = new ArrayList<>();
        for (Map<String, Object> skuInfo : skuList) {
            GoodsSku goodsSku = new GoodsSku(goods);
            builderSingle(goodsSku, skuInfo);
            goodsSkus.add(goodsSku);
        }
        return goodsSkus;
    }


    private static void builderSingle(GoodsSku goodsSku, Map<String, Object> skuInfo) {
        Assert.notNull(goodsSku, "goodsSku不能为空");
        Assert.notEmpty(skuInfo, "skuInfo不能为空");
        //规格简短信息
        StringBuilder simpleSpecs = new StringBuilder();
        //商品名称
        StringBuilder goodsName = new StringBuilder(goodsSku.getGoodsName());
        //规格值
        Map<String, Object> specMap = new LinkedHashMap<>();

        // 原始规格项
        String[] ignoreOriginKeys = {"id", "sn", "cost", "price", "quantity", "weight"};
        //获取规格信息
        for (Map.Entry<String, Object> spec : skuInfo.entrySet()) {
            //保存新增规格信息
            if (!CollUtil.contains(Arrays.asList(ignoreOriginKeys), spec.getKey()) && spec.getValue() != null) {
                specMap.put(spec.getKey(), spec.getValue());
                if (!spec.getKey().equals(IMAGES_KEY)) {
                    //设置商品名称
                    goodsName.append(" ").append(spec.getValue());
                    //规格简短信息
                    simpleSpecs.append(" ").append(spec.getValue());
                }
            }
        }
        //设置规格信息
        goodsSku.setGoodsName(goodsName.toString());


        //规格信息
        goodsSku.setId(Convert.toStr(skuInfo.get("id"), ""));
        goodsSku.setSn(Convert.toStr(skuInfo.get("sn")));
        goodsSku.setWeight(Convert.toDouble(skuInfo.get("weight"), 0D));
        goodsSku.setPrice(Convert.toDouble(skuInfo.get("price"), 0D));
        goodsSku.setCost(Convert.toDouble(skuInfo.get("cost"), 0D));
        goodsSku.setQuantity(Convert.toInt(skuInfo.get("quantity"), 0));
        goodsSku.setSpecs(JSONUtil.toJsonStr(specMap));
        goodsSku.setSimpleSpecs(simpleSpecs.toString());
    }


}
