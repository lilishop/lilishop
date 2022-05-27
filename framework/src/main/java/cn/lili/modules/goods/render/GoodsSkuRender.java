package cn.lili.modules.goods.render;

import cn.lili.modules.goods.entity.dos.GoodsSku;

import java.util.List;
import java.util.Map;

/**
 * 商品sku渲染器
 *
 * @author paulG
 * @since 2022/5/20
 **/
public interface GoodsSkuRender {


    GoodsSku render(List<Map<String, Object>> skuList);

}
