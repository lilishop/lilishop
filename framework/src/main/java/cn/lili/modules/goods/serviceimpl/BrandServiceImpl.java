package cn.lili.modules.goods.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Brand;
import cn.lili.modules.goods.entity.dos.CategoryBrand;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dto.BrandPageDTO;
import cn.lili.modules.goods.entity.vos.BrandVO;
import cn.lili.modules.goods.mapper.BrandMapper;
import cn.lili.modules.goods.service.BrandService;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;


/**
 * 商品品牌业务层实现
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
@Service
public class BrandServiceImpl extends ServiceImpl<BrandMapper, Brand> implements BrandService {

    /**
     * 分类品牌绑定
     */
    @Autowired
    private CategoryBrandService categoryBrandService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private GoodsService goodsService;

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
        //如果是要禁用，则需要先判定绑定关系
        if (Boolean.TRUE.equals(disable)) {
            List<String> ids = new ArrayList<>();
            ids.add(brandId);
            checkBind(ids);
        }
        brand.setDeleteFlag(disable);
        return updateById(brand);
    }

    @Override
    public void deleteBrands(List<String> ids) {
        checkBind(ids);
        this.removeByIds(ids);
    }


    /**
     * 校验绑定关系
     *
     * @param brandIds 品牌Ids
     */
    private void checkBind(List<String> brandIds) {
        //分了绑定关系查询
        List<CategoryBrand> categoryBrands = categoryBrandService.getCategoryBrandListByBrandId(brandIds);
        if (!categoryBrands.isEmpty()) {
            List<String> categoryIds = categoryBrands.stream().map(CategoryBrand::getCategoryId).collect(Collectors.toList());
            throw new ServiceException(ResultCode.BRAND_USE_DISABLE_ERROR,
                    JSONUtil.toJsonStr(categoryService.getCategoryNameByIds(categoryIds)));
        }

        //分了商品绑定关系查询
        List<Goods> goods = goodsService.getByBrandIds(brandIds);
        if (!goods.isEmpty()) {
            List<String> goodsNames = goods.stream().map(Goods::getGoodsName).collect(Collectors.toList());
            throw new ServiceException(ResultCode.BRAND_BIND_GOODS_ERROR,
                    JSONUtil.toJsonStr(goodsNames));
        }
    }

    /**
     * 校验是否存在
     *
     * @param brandId 品牌ID
     * @return 品牌
     */
    private Brand checkExist(String brandId) {
        Brand brand = getById(brandId);
        if (brand == null) {
            log.error("品牌ID为" + brandId + "的品牌不存在");
            throw new ServiceException(ResultCode.BRAND_NOT_EXIST);
        }
        return brand;
    }

}