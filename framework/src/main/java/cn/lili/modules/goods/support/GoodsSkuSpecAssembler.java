package cn.lili.modules.goods.support;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.vos.GoodsSkuSpecVO;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.SpecValueVO;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * 商品 SKU 规格与视图对象组装器
 * <p>
 * 负责 SKU 的规格 JSON 解析、相册映射、视图转换及多规格聚合，解耦 SKU 核心服务中的数据组装细节。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class GoodsSkuSpecAssembler {

    /**
     * 将 SKU 列表批量转换为 SKU 视图对象列表
     *
     * @param list SKU 实体列表
     * @return SKU 视图对象列表
     */
    public List<GoodsSkuVO> toVOList(List<GoodsSku> list) {
        if (list == null || list.isEmpty()) {
            return Collections.emptyList();
        }
        List<GoodsSkuVO> goodsSkuVOS = new ArrayList<>(list.size());
        for (GoodsSku goodsSku : list) {
            goodsSkuVOS.add(toVO(goodsSku));
        }
        return goodsSkuVOS;
    }

    /**
     * 将单个 SKU 实体转换为包含规格与相册的 SKU 视图对象
     *
     * @param goodsSku SKU 实体
     * @return SKU 视图对象
     */
    public GoodsSkuVO toVO(GoodsSku goodsSku) {
        if (goodsSku == null) {
            return null;
        }
        GoodsSkuVO goodsSkuVO = new GoodsSkuVO(goodsSku);
        if (goodsSku.getSpecs() == null) {
            goodsSkuVO.setGoodsGalleryList(Collections.emptyList());
            goodsSkuVO.setSpecList(Collections.emptyList());
            return goodsSkuVO;
        }

        JSONObject jsonObject = JSON.parseObject(goodsSku.getSpecs());
        List<SpecValueVO> specValueVOS = new ArrayList<>();
        List<String> goodsGalleryList = new ArrayList<>();

        if (jsonObject != null) {
            for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
                SpecValueVO specValueVO = new SpecValueVO();
                if ("images".equals(entry.getKey())) {
                    specValueVO.setSpecName(entry.getKey());
                    if (entry.getValue() instanceof JSONArray) {
                        JSONArray jsonArray = (JSONArray) entry.getValue();
                        List<String> specImages = jsonArray.toJavaList(String.class);
                        specValueVO.setSpecImage(specImages);
                        goodsGalleryList = new ArrayList<>(specImages);
                    }
                } else {
                    specValueVO.setSpecName(entry.getKey());
                    specValueVO.setSpecValue(entry.getValue() != null ? entry.getValue().toString() : null);
                }
                specValueVOS.add(specValueVO);
            }
        }
        goodsSkuVO.setGoodsGalleryList(goodsGalleryList);
        goodsSkuVO.setSpecList(specValueVOS);
        return goodsSkuVO;
    }

    /**
     * 将 SKU 视图对象列表按规格信息聚合成规格分组列表
     *
     * @param goodsSkuVOList 商品 SKU 视图列表
     * @return SKU 规格分组视图列表
     */
    public List<GoodsSkuSpecVO> groupBySkuAndSpec(List<GoodsSkuVO> goodsSkuVOList) {
        if (goodsSkuVOList == null || goodsSkuVOList.isEmpty()) {
            return Collections.emptyList();
        }
        List<GoodsSkuSpecVO> skuSpecVOList = new ArrayList<>(goodsSkuVOList.size());
        for (GoodsSkuVO goodsSkuVO : goodsSkuVOList) {
            GoodsSkuSpecVO specVO = new GoodsSkuSpecVO();
            specVO.setSkuId(goodsSkuVO.getId());
            specVO.setSpecValues(goodsSkuVO.getSpecList());
            specVO.setQuantity(goodsSkuVO.getQuantity());
            skuSpecVOList.add(specVO);
        }
        return skuSpecVOList;
    }
}
