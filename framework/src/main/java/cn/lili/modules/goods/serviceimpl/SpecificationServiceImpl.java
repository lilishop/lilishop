package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.CategorySpecification;
import cn.lili.modules.goods.entity.dos.SpecValues;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.entity.dto.SpecificationSearchParams;
import cn.lili.modules.goods.entity.vos.CategorySpecificationVO;
import cn.lili.modules.goods.entity.vos.GoodsSpecValueVO;
import cn.lili.modules.goods.entity.vos.SpecificationVO;
import cn.lili.modules.goods.mapper.SpecificationMapper;
import cn.lili.modules.goods.service.CategorySpecificationService;
import cn.lili.modules.goods.service.SpecValuesService;
import cn.lili.modules.goods.service.SpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 商品规格业务层实现
 *
 * @author pikachu
 * @date 2020-02-18 16:18:56
 */
@Service
@Transactional
public class SpecificationServiceImpl extends ServiceImpl<SpecificationMapper, Specification> implements SpecificationService {

    //分类-规格绑定
    @Autowired
    private CategorySpecificationService categorySpecificationService;
    //规格值
    @Autowired
    private SpecValuesService specValuesService;

    @Override
    public List<SpecificationVO> getSpecList(String specId) {

        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq(StringUtils.isNotEmpty(specId), "s.spec_id", specId);
        queryWrapper.orderByDesc("s.create_time");
        return this.baseMapper.findSpecList(queryWrapper);
    }

    @Override
    public List<GoodsSpecValueVO> getGoodsSpecValue(String categoryId) {
        List<CategorySpecificationVO> categorySpecificationVOS = categorySpecificationService.getCategorySpecList(categoryId);
        Map<String, Object> map = new HashMap<>();
        if (!categorySpecificationVOS.isEmpty()) {
            //循环组织查询规格值数据
            List<String> valueId = new ArrayList<>();
            for (CategorySpecificationVO categorySpecification : categorySpecificationVOS) {
                map.put(categorySpecification.getId(), categorySpecification.getName());
                valueId.add(categorySpecification.getId());
            }
            //使用valueId去查询规格值
            List<SpecValues> specValues = specValuesService.getSpecValues(valueId);
            //循环组织数据
            List<GoodsSpecValueVO> goodsSpecValueVOS = new ArrayList<>();
            for (Map.Entry<String, Object> m : map.entrySet()) {
                GoodsSpecValueVO goodsSpecValueVO = new GoodsSpecValueVO();
                goodsSpecValueVO.setName(m.getValue().toString());
                List<String> list = new ArrayList<>();
                for (SpecValues spec : specValues) {
                    if (spec.getSpecId().equals(m.getKey())) {
                        list.add(spec.getSpecValue());
                    }
                }
                goodsSpecValueVO.setValue(list);
                goodsSpecValueVOS.add(goodsSpecValueVO);
            }
            return goodsSpecValueVOS;
        }
        return new ArrayList<>();
    }

    @Override
    public Specification getSpecification(String id) {
        Specification specification = this.getById(id);
        if (specification == null) {
            throw new ServiceException("当前商品已下架");
        }
        return specification;
    }

    @Override
    public IPage<SpecificationVO> getSpecificationPage(SpecificationSearchParams searchParams, PageVO pageVo) {

        List<SpecificationVO> specList = this.getSpecList(searchParams.getSpecId());
        IPage<SpecificationVO> page = new Page<>(pageVo.getPageNumber(), pageVo.getPageSize(), specList.size());
        page.setRecords(PageUtil.listToPage(pageVo, specList));
        return page;
    }

    @Override
    public IPage<Specification> getSpecificationByPage(SpecificationSearchParams searchParams, PageVO pageVo) {
        List<String> specIds = new ArrayList<>();
        if (StrUtil.isNotEmpty(searchParams.getCategoryPath())) {
            String categoryPath = searchParams.getCategoryPath();
            List<CategorySpecification> categorySpecList = categorySpecificationService.getCategorySpecList(categoryPath.split(","));
            categorySpecList.forEach(i -> specIds.add(i.getSpecificationId()));
        }
        QueryWrapper<Specification> queryWrapper = searchParams.queryWrapper();
        queryWrapper.in("id", specIds);
        return this.page(PageUtil.initPage(pageVo), queryWrapper);
    }

    @Override
    public Specification addSpecification(SpecificationVO specificationVO) {
        Specification specification = this.getOne(new LambdaQueryWrapper<Specification>().eq(Specification::getSpecName, specificationVO.getSpecName()));
        if (specification == null) {
            this.save(specificationVO);
            specification = specificationVO;
        }

        CategorySpecification categorySpecification = categorySpecificationService.getOne(new LambdaQueryWrapper<CategorySpecification>().eq(CategorySpecification::getSpecificationId, specification.getId()));
        if (categorySpecification == null) {
            categorySpecification = new CategorySpecification();
            categorySpecification.setSpecificationId(specification.getId());
            String categoryPath = specificationVO.getCategoryPath();
            if (CharSequenceUtil.isNotEmpty(categoryPath)) {
                categorySpecification.setCategoryId(categoryPath.substring(categoryPath.lastIndexOf(",") + 1));
                categorySpecificationService.save(categorySpecification);
            }
        }
        if (CharSequenceUtil.isNotEmpty(specificationVO.getSpecValue())) {
            specValuesService.saveSpecValue(specificationVO.getId(), new String[]{specificationVO.getSpecValue()});
        }
        return specification;
    }

    @Override
    public boolean updateSpecification(SpecificationVO specificationVO) {
        this.getSpecification(specificationVO.getId());
        return this.updateById(specificationVO);
    }

    @Override
    public boolean deleteSpecification(List<String> ids) {
        for (String id : ids) {
            //如果此规格绑定分类则不允许删除
            List<CategorySpecification> list = categorySpecificationService.list(new QueryWrapper<CategorySpecification>().eq("specification_id", id));
            if (!list.isEmpty()) {
                throw new ServiceException(ResultCode.SPEC_DELETE_ERROR);
            }
            //删除规格
            this.removeById(id);
            //删除规格值
            specValuesService.remove(new QueryWrapper<SpecValues>().eq("spec_id", id));
        }
        return true;
    }

}