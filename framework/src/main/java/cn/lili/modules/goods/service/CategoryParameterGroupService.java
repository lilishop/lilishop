package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dos.CategoryParameterGroup;
import cn.lili.modules.goods.entity.vos.ParameterGroupVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 分类绑定参数组业务层
 *
 * @author pikachu
 * @date 2020-03-02 16:44:56
 */
public interface CategoryParameterGroupService extends IService<CategoryParameterGroup> {

    /**
     * 查询分类绑定参数集合
     *
     * @param categoryId 分类Id
     * @return 分类参数
     */
    List<ParameterGroupVO> getCategoryParams(String categoryId);


    /**
     * 查询分类绑定参数组信息
     *
     * @param categoryId 分类id
     * @return 参数组列表
     */
    List<CategoryParameterGroup> getCategoryGroup(String categoryId);
    
}