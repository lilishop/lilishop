package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dos.CategorySpecification;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 商品分类规格业务层
 *
 * @author pikachu
 * @since 2020-02-26 16:18:56
 */
public interface CategorySpecificationService extends IService<CategorySpecification> {

    /**
     * 通过分类ID删除关联规格
     *
     * @param categoryId 分类ID
     */
    void deleteByCategoryId(String categoryId);
}
