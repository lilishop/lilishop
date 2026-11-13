package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CategoryParameter;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsSearchDTO;
import cn.lili.modules.goods.mapper.ParametersMapper;
import cn.lili.modules.goods.service.CategoryParameterService;
import cn.lili.modules.goods.service.ParametersService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 商品参数定义及分类关联服务，不回写商品已保存的参数内容或发送商品索引消息。
 *
 * @author pikachu
 * @since 2020-03-02 16:18:56
 */
@Service
public class ParametersServiceImpl extends ServiceImpl<ParametersMapper, Parameters> implements ParametersService {

    @Autowired
    private CategoryParameterService categoryParameterService;

    /**
     * 按参数名称模糊分页查询商品参数定义，并按创建时间倒序排列。
     *
     * @param goodsParamsSearchDTO 参数名称及分页条件
     * @return 商品参数分页结果
     */
    @Override
    public IPage<Parameters> parametersPage(GoodsParamsSearchDTO goodsParamsSearchDTO) {
        LambdaQueryWrapper<Parameters> queryWrapper = new LambdaQueryWrapper<>();
        if (StrUtil.isNotBlank(goodsParamsSearchDTO.getParamName())) {
            queryWrapper.like(Parameters::getParamName, goodsParamsSearchDTO.getParamName());
        }

        queryWrapper.orderByDesc(Parameters::getCreateTime);
        return this.page(PageUtil.initPage(goodsParamsSearchDTO), queryWrapper);
    }

    /**
     * 保存商品参数定义后维护分类关联，返回参数记录的保存结果。
     *
     * @param goodsParamsDTO 商品参数定义及分类关联列表
     * @return 参数记录是否保存成功
     */
    @Override
    public boolean addParameter(GoodsParamsDTO goodsParamsDTO) {

        Parameters parameters = new Parameters();
        BeanUtils.copyProperties(goodsParamsDTO, parameters);
        Boolean isSave = this.save(parameters);
        List<String> categoryIds = new ArrayList<>();
        for (CategoryParameter categoryParameter : goodsParamsDTO.getCategoryParameterList()) {
            categoryIds.add(categoryParameter.getCategoryId());
        }
        categoryParameterService.saveCategoryParameterList(parameters.getId(), categoryIds);

        return isSave;
    }

    /**
     * 校验参数定义存在后更新其内容及分类关联，两类写入参与同一事务。
     * 保留商品已有参数内容，不触发历史商品回写或索引消息。
     *
     * @param goodsParamsDTO 参数信息
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateParameter(GoodsParamsDTO goodsParamsDTO) {
        Parameters origin = this.getById(goodsParamsDTO.getId());
        if (origin == null) {
            throw new ServiceException(ResultCode.CATEGORY_NOT_EXIST);
        }

        Boolean isUpdate = this.updateById(goodsParamsDTO);
        List<String> categoryIds = new ArrayList<>();
        for (CategoryParameter categoryParameter : goodsParamsDTO.getCategoryParameterList()) {
            categoryIds.add(categoryParameter.getCategoryId());
        }
        categoryParameterService.saveCategoryParameterList(goodsParamsDTO.getId(), categoryIds);
        return isUpdate;
    }

    /**
     * 读取商品参数定义及其当前关联的分类列表。
     *
     * @param id 商品参数定义主键
     * @return 参数定义及分类关联信息
     */
    @Override
    public GoodsParamsDTO getGoodsParamsDTO(String id) {

        GoodsParamsDTO goodsParamsDTO = new GoodsParamsDTO();
        Parameters parameters = this.getById(id);
        BeanUtils.copyProperties(parameters, goodsParamsDTO);
        goodsParamsDTO.setCategoryParameterList(categoryParameterService.getCategoryParamListByParameterId(parameters.getId()));
        return goodsParamsDTO;
    }

    /**
     * 根据分类关联查询商品参数定义，无关联时直接返回空列表。
     *
     * @param categoryId 商品分类主键
     * @return 该分类关联的参数定义列表
     */
    @Override
    public List<Parameters> getParametersByCategoryId(String categoryId) {
        List<CategoryParameter> categoryParameterList = categoryParameterService.getCategoryParamList(categoryId);
        List<String> parametersId = new ArrayList<>();
        for (CategoryParameter categoryParameter : categoryParameterList) {
            parametersId.add(categoryParameter.getParameterId());
        }
        if (parametersId.isEmpty()) {
            return Collections.emptyList();
        }
        return this.list(new LambdaQueryWrapper<Parameters>().in(Parameters::getId, parametersId));
    }
}
