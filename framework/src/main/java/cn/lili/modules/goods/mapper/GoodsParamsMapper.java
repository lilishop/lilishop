package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.GoodsParams;
import cn.lili.modules.goods.entity.vos.GoodsParamsVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;


/**
 * 商品关联参数数据处理层
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface GoodsParamsMapper extends BaseMapper<GoodsParams> {

    @Select("select p.*,gp.param_value,p.group_id  from li_parameters p left join li_goods_params gp on p.id=gp.param_id and gp.goods_id = #{goodsId}  where p.category_id = #{categoryId} order by sort")
    List<GoodsParamsVO> paramList(String goodsId, String categoryId);

}