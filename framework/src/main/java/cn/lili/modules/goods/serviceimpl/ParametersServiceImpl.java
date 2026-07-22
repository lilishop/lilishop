package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.StrUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.CategoryParameter;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsSearchDTO;
import cn.lili.modules.goods.mapper.ParametersMapper;
import cn.lili.modules.goods.service.CategoryParameterService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.ParametersService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import cn.lili.message.LiliMessageTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * 商品参数业务层实现
 *
 * @author pikachu
 * @since 2020-03-02 16:18:56
 */
@Service
public class ParametersServiceImpl extends ServiceImpl<ParametersMapper, Parameters> implements ParametersService {


    @Autowired
    @Lazy
    private GoodsService goodsService;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private CategoryParameterService categoryParameterService;

    @Override
    public IPage<Parameters> parametersPage(GoodsParamsSearchDTO goodsParamsSearchDTO) {
        LambdaQueryWrapper<Parameters> queryWrapper = new LambdaQueryWrapper<>();
        if (StrUtil.isNotBlank(goodsParamsSearchDTO.getParamName())) {
            queryWrapper.like(Parameters::getParamName, goodsParamsSearchDTO.getParamName());
        }

        queryWrapper.orderByDesc(Parameters::getCreateTime);
        return this.page(PageUtil.initPage(goodsParamsSearchDTO), queryWrapper);
    }

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
     * 更新参数组信息
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

        List<String> goodsIds = new ArrayList<>();
//        LambdaQueryWrapper<Goods> queryWrapper = new LambdaQueryWrapper<>();
//        queryWrapper.select(Goods::getId, Goods::getParams);
//        queryWrapper.like(Goods::getParams, parameters.getId());
//        List<Map<String, Object>> goodsList = this.goodsService.listMaps(queryWrapper);
//
//        if (!goodsList.isEmpty()) {
//            for (Map<String, Object> goods : goodsList) {
//                String params = (String) goods.get("params");
//                List<GoodsParamsDTO> goodsParamsDTOS = JSON.parseArray(params, GoodsParamsDTO.class);
//                this.setGoodsItemDTOList(goodsParamsDTOS, parameters);
//                this.goodsService.updateGoodsParams(goods.get("id").toString(), JSON.toJSONString(goodsParamsDTOS));
//                goodsIds.add(goods.get("id").toString());
//            }
//
//            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.UPDATE_GOODS_INDEX.name();
//            //发送mq消息
//            liliMessageTemplate.asyncSend(destination, JSON.toJSONString(goodsIds), MessageSendCallbackBuilder.commonCallback());
//        }

        Boolean isUpdate = this.updateById(goodsParamsDTO);
        List<String> categoryIds = new ArrayList<>();
        for (CategoryParameter categoryParameter : goodsParamsDTO.getCategoryParameterList()) {
            categoryIds.add(categoryParameter.getCategoryId());
        }
        categoryParameterService.saveCategoryParameterList(goodsParamsDTO.getId(), categoryIds);
        return isUpdate;
    }

    @Override
    public GoodsParamsDTO getGoodsParamsDTO(String id) {

        GoodsParamsDTO goodsParamsDTO = new GoodsParamsDTO();
        Parameters parameters = this.getById(id);
        BeanUtils.copyProperties(parameters, goodsParamsDTO);
        goodsParamsDTO.setCategoryParameterList(categoryParameterService.getCategoryParamListByParameterId(parameters.getId()));
        return goodsParamsDTO;
    }

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

//    /**
//     * 更新商品参数信息
//     *
//     * @param goodsParamsDTOList 商品参数项列表
//     * @param parameters         参数信息
//     */
//    private void setGoodsItemDTOList(List<GoodsParamsDTO> goodsParamsDTOList, Parameters parameters) {
//        for (GoodsParamsDTO goodsParamsDTO : goodsParamsDTOList) {
//            List<GoodsParamsItemDTO> goodsParamsItemDTOList = goodsParamsDTO.getGoodsParamsItemDTOList().stream().filter(i -> i.getParamId() != null && i.getParamId().equals(parameters.getId())).collect(Collectors.toList());
//            for (GoodsParamsItemDTO goodsParamsItemDTO : goodsParamsItemDTOList) {
//                this.setGoodsItemDTO(goodsParamsItemDTO, parameters);
//            }
//        }
//    }

    /**
     * 更新商品参数详细信息
     *
     * @param goodsParamsItemDTO 商品参数项信息
     * @param parameters         参数信息
     */
    private void setGoodsItemDTO(GoodsParamsItemDTO goodsParamsItemDTO, Parameters parameters) {
        if (goodsParamsItemDTO.getParamId().equals(parameters.getId())) {
            goodsParamsItemDTO.setParamId(parameters.getId());
            goodsParamsItemDTO.setParamName(parameters.getParamName());
            goodsParamsItemDTO.setRequired(parameters.getRequired());
            goodsParamsItemDTO.setIsIndex(parameters.getIsIndex());
            goodsParamsItemDTO.setSort(parameters.getSort());
            if (CharSequenceUtil.isNotEmpty(parameters.getOptions()) && CharSequenceUtil.isNotEmpty(goodsParamsItemDTO.getParamValue()) && !parameters.getOptions().contains(goodsParamsItemDTO.getParamValue())) {
                if (parameters.getOptions().contains(",")) {
                    goodsParamsItemDTO.setParamValue(parameters.getOptions().substring(0, parameters.getOptions().indexOf(",")));
                } else {
                    goodsParamsItemDTO.setParamValue(parameters.getOptions());
                }
            }
        }
    }

}
