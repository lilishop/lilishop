package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.CategoryBrand;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 商品分类品牌数据处理层
 *
 * @author pikachu
 * @since 2020-02-26 18:12:56
 */
public interface CategoryBrandMapper extends BaseMapper<CategoryBrand> {

    /**
     * 根据分类id查分类绑定品牌
     *
     * @param categoryId 分类id
     * @return 分类绑定的品牌列表
     */
    @Select("SELECT b.id,b.name FROM li_brand b INNER join li_category_brand cb on b.id = cb.brand_id and cb.category_id = #{categoryId} where b.delete_flag = 0")
    List<CategoryBrandVO> getCategoryBrandList(String categoryId);
}