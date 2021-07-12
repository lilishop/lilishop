package cn.lili.modules.goods.mapper;

import cn.lili.modules.goods.entity.dos.Category;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;


/**
 * 商品分类数据处理层
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface CategoryMapper extends BaseMapper<Category> {

    /**
     * 获取分类名称列表
     *
     * @param queryWrapper 查询条件
     * @return 分类名称列表
     */
    @Select("SELECT name FROM li_category ${ew.customSqlSegment} ")
    List<String> getNamesByIds(@Param(Constants.WRAPPER) Wrapper<Category> queryWrapper);

}
