package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.GoodsCollection;
import cn.lili.modules.member.entity.vo.GoodsCollectionVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 会员收藏数据处理层
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
public interface GoodsCollectionMapper extends BaseMapper<GoodsCollection> {

    /**
     * 商品收藏VO分页
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 商品收藏VO分页
     */
    @Select("select gc.id AS id,gs.id as sku_id,gs.goods_id as goods_id,gs.goods_name as goods_name,gs.thumbnail as image,gs.price,gs.market_enable AS market_enable from li_goods_collection gc INNER JOIN li_goods_sku gs ON gc.sku_id=gs.id    ${ew.customSqlSegment} ")
    IPage<GoodsCollectionVO> goodsCollectionVOList(IPage<GoodsCollectionVO> page, @Param(Constants.WRAPPER) Wrapper<GoodsCollectionVO> queryWrapper);

}