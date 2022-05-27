package cn.lili.modules.goods.sku;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.lang.Assert;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
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
     * @param goods             商品
     * @param skuInfo           sku信息列表
     * @param goodsOperationDTO 商品操作信息（如需处理额外信息传递，不需可传空）
     * @return 商品sku
     */
    public static GoodsSku build(Goods goods, Map<String, Object> skuInfo, GoodsOperationDTO goodsOperationDTO) {
        GoodsSku goodsSku = new GoodsSku(goods);
        builderSingle(goodsSku, skuInfo, goodsOperationDTO);
        return goodsSku;
    }

    /**
     * 批量构建商品sku
     *
     * @param goods             商品
     * @param goodsOperationDTO 商品操作信息
     * @return 商品sku
     */
    public static List<GoodsSku> buildBatch(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        return builderBatch(goods, goodsOperationDTO);
    }

    /**
     * 从已有的商品sku中构建商品sku
     *
     * @param goodsSku          原商品sku
     * @param skuInfo           sku信息列表
     * @param goodsOperationDTO 商品操作信息（如需处理额外信息传递，不需可传空）
     * @return 商品sku
     */
    public static GoodsSku build(GoodsSku goodsSku, Map<String, Object> skuInfo, GoodsOperationDTO goodsOperationDTO) {
        builderSingle(goodsSku, skuInfo, goodsOperationDTO);
        return goodsSku;
    }

    private static void builderSingle(GoodsSku goodsSku, Map<String, Object> skuInfo, GoodsOperationDTO goodsOperationDTO) {
        Assert.notNull(goodsSku, "goodsSku不能为空");
        Assert.notEmpty(skuInfo, "skuInfo不能为空");
        //规格简短信息
        StringBuilder simpleSpecs = new StringBuilder();
        //商品名称
        StringBuilder goodsName = new StringBuilder(goodsOperationDTO.getGoodsName());
        //规格值
        Map<String, Object> specMap = new HashMap<>(16);

        // 原始规格项
        String[] ignoreOriginKeys = {"id", "sn", "cost", "price", "quantity", "weight", IMAGES_KEY};
        //获取规格信息
        for (Map.Entry<String, Object> spec : skuInfo.entrySet()) {
            //保存新增规格信息
            if (!CollUtil.contains(Arrays.asList(ignoreOriginKeys), spec.getKey()) && spec.getValue() != null) {
                specMap.put(spec.getKey(), spec.getValue());
                //设置商品名称
                goodsName.append(" ").append(spec.getValue());
                //规格简短信息
                simpleSpecs.append(" ").append(spec.getValue());
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

    private static List<GoodsSku> builderBatch(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        Assert.notEmpty(goodsOperationDTO.getSkuList(), "goodsOperationDTO.getSkuList()不能为空");
        Assert.notNull(goods, "goods不能为空");
        List<GoodsSku> goodsSkus = new ArrayList<>();
        for (Map<String, Object> skuInfo : goodsOperationDTO.getSkuList()) {
            GoodsSku goodsSku = new GoodsSku(goods);
            builderSingle(goodsSku, skuInfo, goodsOperationDTO);
            goodsSkus.add(goodsSku);
        }
        return goodsSkus;
    }


}
