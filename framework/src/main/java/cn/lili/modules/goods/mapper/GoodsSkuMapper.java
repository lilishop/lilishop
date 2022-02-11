package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.GoodsSku;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 规格项数据处理层
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
public interface GoodsSkuMapper extends BaseMapper<GoodsSku> {

    /**
     * 根据商品id获取全部skuId的集合
     *
     * @param goodsId goodsId
     * @return 全部skuId的集合
     */
    @Select("SELECT id FROM li_goods_sku WHERE goods_id = #{goodsId}")
    List<String> getGoodsSkuIdByGoodsId(String goodsId);

}