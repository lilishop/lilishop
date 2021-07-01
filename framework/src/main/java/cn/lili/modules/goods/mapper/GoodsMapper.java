package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import java.util.List;

/**
 * 规格项数据处理层
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface GoodsMapper extends BaseMapper<Goods> {

    /**
     * 根据店铺ID获取商品ID列表
     *
     * @param storeId 店铺ID
     */
    @Select("SELECT id FROM li_goods WHERE store_id = #{storeId}")
    List<String> getGoodsIdByStoreId(String storeId);

    @Update("UPDATE li_goods SET comment_num = comment_num + #{commentNum} WHERE id = #{goodsId}")
    void addGoodsCommentNum(Integer commentNum, String goodsId);

    @Select("select g.* from li_goods as g ")
    IPage<GoodsVO> queryByParams(IPage<GoodsVO> page, @Param(Constants.WRAPPER) Wrapper<GoodsVO> queryWrapper);
}