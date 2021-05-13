package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.SpecValues;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.mapper.SpecValuesMapper;
import cn.lili.modules.goods.service.SpecValuesService;
import cn.lili.modules.goods.service.SpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 规格项接口实现
 *
 * @author pikachu
 * @date 2020-02-18 16:18:56
 */

@Service
@Transactional
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class SpecValuesServiceImpl extends ServiceImpl<SpecValuesMapper, SpecValues> implements SpecValuesService {

    //规格
    private SpecificationService specificationService;

    @Override
    public List<SpecValues> saveSpecValue(String specId, String[] valueList) {
        //校验是否存在
        Specification specification = specificationService.getById(specId);
        List<SpecValues> res = new ArrayList<>();
        if (specification != null) {
            //先删除原有规格值
            QueryWrapper<SpecValues> queryWrapper = new QueryWrapper<>();
            queryWrapper.eq("spec_id", specId);
            this.remove(queryWrapper);
            //添加新的规格值
            for (String value : valueList) {
                SpecValues specValues = new SpecValues();
                specValues.setSpecValue(value);
                specValues.setSpecId(specification.getId());
                this.save(specValues);
                res.add(specValues);
            }
            return res;
        }
        return res;
    }

    @Override
    public List<SpecValues> addSpecValue(String specId, String[] valueList) {
        List<SpecValues> specValuesList = new ArrayList<>();
        for (String value : valueList) {
            QueryWrapper<SpecValues> queryWrapper = new QueryWrapper<>();
            queryWrapper.eq("spec_id", specId);
            queryWrapper.eq("spec_value", value);
            if (this.getOne(queryWrapper) == null) {
                SpecValues specValues = new SpecValues();
                specValues.setSpecValue(value);
                specValues.setSpecId(specId);
                this.save(specValues);
                specValuesList.add(specValues);
            }
        }
        return specValuesList;
    }

    @Override
    public List<SpecValues> getSpecValues(List<String> specIds) {
        LambdaQueryWrapper<SpecValues> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(SpecValues::getSpecValue, specIds);
        return this.list(queryWrapper);
    }

    @Override
    public SpecValues getSpecValues(String specValue, String specId) {
        LambdaQueryWrapper<SpecValues> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(SpecValues::getSpecValue, specValue);
        queryWrapper.eq(SpecValues::getSpecId, specId);

        SpecValues specValues = this.getOne(queryWrapper);
        if (specValues == null) {
            specValues = new SpecValues();
            specValues.setSpecValue(specValue);
            specValues.setSpecId(specId);
            this.save(specValues);
        }
        return specValues;
    }

    @Override
    public IPage<SpecValues> queryByParams(String specId, String specVal, PageVO pageVo) {
        LambdaQueryWrapper<SpecValues> queryWrapper = new LambdaQueryWrapper<>();
        if (StrUtil.isNotEmpty(specId)) {
            queryWrapper.eq(SpecValues::getSpecId, specId);
        }
        if (StrUtil.isNotEmpty(specVal)) {
            queryWrapper.like(SpecValues::getSpecValue, specVal);
        }
        return this.page(PageUtil.initPage(pageVo), queryWrapper);
    }

    @Autowired
    public void setSpecificationService(SpecificationService specificationService) {
        this.specificationService = specificationService;
    }
}