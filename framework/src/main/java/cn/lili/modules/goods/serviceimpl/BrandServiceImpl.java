package cn.lili.modules.goods.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.PageUtil;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.CategoryBrand;
import cn.lili.modules.goods.entity.dto.BrandPageDTO;
import cn.lili.modules.goods.entity.vos.BrandVO;
import cn.lili.modules.goods.mapper.BrandMapper;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryBrandService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


/**
 * 商品品牌业务层实现
 *
 * @author pikachu
 * @date 2020-02-18 16:18:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class BrandServiceImpl extends ServiceImpl<BrandMapper, Brand> implements BrandService {

    /**
     * 分类品牌绑定
     */
    @Autowired
    private CategoryBrandService categoryBrandService;

    @Override
    public IPage<Brand> getBrandsByPage(BrandPageDTO page) {
        LambdaQueryWrapper<Brand> queryWrapper = new LambdaQueryWrapper<>();
        if (page.getName() != null) {
            queryWrapper.like(Brand::getName, page.getName());
        }
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

    @Override
    public List<Brand> getBrandsByCategory(String categoryId) {
        QueryWrapper<CategoryBrand> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("category_id", categoryId);
        List<CategoryBrand> list = categoryBrandService.list(queryWrapper);
        if (list != null && !list.isEmpty()) {
            List<String> collect = list.stream().map(CategoryBrand::getBrandId).collect(Collectors.toList());
            return this.list(new LambdaQueryWrapper<Brand>().in(Brand::getId, collect));
        }
        return new ArrayList<>();
    }

    @Override
    public boolean addBrand(BrandVO brandVO) {

        if (getOne(new LambdaQueryWrapper<Brand>().eq(Brand::getName, brandVO.getName())) != null) {
            throw new ServiceException(ResultCode.BRAND_NAME_EXIST_ERROR);
        }
        return this.save(brandVO);
    }

    @Override
    public boolean updateBrand(BrandVO brandVO) {
        this.checkExist(brandVO.getId());
        if (getOne(new LambdaQueryWrapper<Brand>().eq(Brand::getName, brandVO.getName()).ne(Brand::getId, brandVO.getId())) != null) {
            throw new ServiceException(ResultCode.BRAND_NAME_EXIST_ERROR);
        }
        return this.updateById(brandVO);
    }

    @Override
    public boolean brandDisable(String brandId, boolean disable) {
        Brand brand = this.checkExist(brandId);
        if (Boolean.TRUE.equals(disable) && !categoryBrandService.getCategoryBrandListByBrandId(brandId).isEmpty()) {
            throw new ServiceException(ResultCode.BRAND_USE_DISABLE_ERROR);
        }
        brand.setDeleteFlag(disable);
        return updateById(brand);
    }

    private Brand checkExist(String brandId) {
        Brand brand = getById(brandId);
        if (brand == null) {
            log.error("品牌ID为" + brandId + "的品牌不存在");
            throw new ServiceException(ResultCode.BRAND_NOT_EXIST);
        }
        return brand;
    }

}