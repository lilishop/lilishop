package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.CategorySpecification;
import cn.lili.modules.goods.entity.vos.CategorySpecificationVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 商品分类规格数据处理层
 *
 * @author pikachu
 * @date 2020-02-27 15:18:56
 */
public interface CategorySpecificationMapper extends BaseMapper<CategorySpecification> {
    /**
     * 根据分类id查分类绑定规格
     *
     * @param categoryId 分类id
     * @return 分类绑定规格列表
     */
    @Select("select s.id, s.spec_name as `name` from " +
            "li_specification s INNER join li_category_specification cs on s.id = cs.specification_id and cs.category_id = #{categoryId} " +
            "where s.delete_flag = 0")
    List<CategorySpecificationVO> getCategorySpecList(String categoryId);
}