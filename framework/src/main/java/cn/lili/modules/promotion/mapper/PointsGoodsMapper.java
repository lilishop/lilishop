package cn.lili.modules.promotion.mapper;

import cn.lili.modules.promotion.entity.dos.PointsGoods;
import cn.lili.modules.promotion.entity.vos.PointsGoodsVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 积分商品数据处理层
 *
 * @author paulG
 * @date 2020/8/21
 **/
public interface PointsGoodsMapper extends BaseMapper<PointsGoods> {

    /**
     * 积分商品分页
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 积分商品分页
     */
    @Select("select * from points_goods pg left join goods_sku gs on pg.sku_id = gs.id ${ew.customSqlSegment}")
    IPage<PointsGoodsVO> getPointsGoodsVO(IPage<PointsGoodsVO> page, @Param(Constants.WRAPPER) Wrapper<PointsGoodsVO> queryWrapper);

}
