package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CategoryBrand;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.mapper.CategoryBrandMapper;
import cn.lili.modules.goods.service.CategoryBrandService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 规格项业务层实现
 *
 * @author pikachu
 * @date 2020-02-18 16:18:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
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
    public List<CategoryBrand> getCategoryBrandListByBrandId(String brandId) {
        return this.list(new LambdaQueryWrapper<CategoryBrand>().eq(CategoryBrand::getBrandId, brandId));
    }
}