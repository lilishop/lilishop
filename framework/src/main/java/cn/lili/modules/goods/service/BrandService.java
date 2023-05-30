package cn.lili.modules.goods.service;


import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dto.BrandPageDTO;
import cn.lili.modules.goods.entity.vos.BrandVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 商品品牌业务层
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
public interface BrandService extends IService<Brand> {

    /**
     * 根据条件分页获取品牌列表
     *
     * @param page 条件参数
     * @return 品牌列表
     */
    IPage<Brand> getBrandsByPage(BrandPageDTO page);

    /**
     * 删除品牌
     *
     * @param ids 品牌id
     */
    void deleteBrands(List<String> ids);

    /**
     * 根据分类ID获取品牌列表
     *
     * @param categoryId 分类ID
     * @return 品牌列表
     */
    List<Brand> getBrandsByCategory(String categoryId);

    /**
     * 根据分类ID获取品牌列表
     *
     * @param categoryIds 分类ID
     * @return 品牌列表
     */
    List<Map<String, Object>> getBrandsMapsByCategory(List<String> categoryIds, String columns);

    /**
     * 添加品牌
     *
     * @param brandVO 品牌信息
     * @return 添加结果
     */
    boolean addBrand(BrandVO brandVO);

    /**
     * 更新品牌
     *
     * @param brandVO 品牌信息
     * @return 更新结果
     */
    boolean updateBrand(BrandVO brandVO);

    /**
     * 更新品牌是否可用
     *
     * @param brandId 品牌ID
     * @param disable 是否不可用
     * @return 更新结果
     */
    boolean brandDisable(String brandId, boolean disable);

}