package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CategoryBrand;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.mapper.CategoryBrandMapper;
import cn.lili.modules.goods.service.CategoryBrandService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 规格项业务层实现
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
@Service
public class CategoryBrandServiceImpl extends ServiceImpl<CategoryBrandMapper, CategoryBrand> implements CategoryBrandService {

    @Override
    public List<CategoryBrandVO> getCategoryBrandList(String categoryId) {
        return this.baseMapper.getCategoryBrandList(categoryId);
    }

    @Override
    public void deleteByCategoryId(String categoryId) {
        this.baseMapper.delete(new LambdaUpdateWrapper<CategoryBrand>().eq(CategoryBrand::getCategoryId, categoryId));
    }

    @Override
    public List<CategoryBrand> getCategoryBrandListByBrandId(List<String> brandId) {
        return this.list(new LambdaQueryWrapper<CategoryBrand>().in(CategoryBrand::getBrandId, brandId));
    }

    @Override
    public void saveCategoryBrandList(String categoryId, List<String> brandIds) {
        //删除分类品牌绑定信息
        this.deleteByCategoryId(categoryId);
        //绑定品牌信息
        if (!brandIds.isEmpty()) {
            List<CategoryBrand> categoryBrands = new ArrayList<>();
            for (String brandId : brandIds) {
                categoryBrands.add(new CategoryBrand(categoryId, brandId));
            }
            this.saveBatch(categoryBrands);
        }
    }
}