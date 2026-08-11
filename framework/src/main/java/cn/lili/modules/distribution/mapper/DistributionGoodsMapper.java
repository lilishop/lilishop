package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 分销商品数据处理层
 *
 * @author pikachu
 * @since 2020-03-24 23:04:56
 */
public interface DistributionGoodsMapper extends BaseMapper<DistributionGoods> {

    /**
     * 分页查询分销商品 VO（库存取自 SKU 实时库存）。
     *
     * @param page         分页参数
     * @param queryWrapper 查询条件
     * @return 分销商品 VO 分页
     */
    @Select("SELECT dg.id, dg.create_by, dg.create_time, dg.goods_id, dg.goods_name, dg.sku_id, dg.specs, "
            + "dg.store_id, dg.store_name, dg.commission, dg.price, dg.thumbnail, dg.auth_status, dg.auth_remark, "
            + "gs.quantity AS quantity, gs.goods_type AS goodsType, gs.category_path AS categoryPath, "
            + "b.name AS brandName "
            + "FROM li_distribution_goods dg "
            + "LEFT JOIN li_goods_sku gs ON dg.sku_id = gs.id AND gs.delete_flag = 0 "
            + "LEFT JOIN li_brand b ON gs.brand_id = b.id AND b.delete_flag = 0 "
            + "${ew.customSqlSegment}")
    IPage<DistributionGoodsVO> getDistributionGoodsVO(IPage<DistributionGoodsVO> page,
                                                      @Param(Constants.WRAPPER) Wrapper<DistributionGoodsVO> queryWrapper);
}
