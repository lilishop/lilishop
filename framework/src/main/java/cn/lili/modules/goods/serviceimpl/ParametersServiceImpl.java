package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.mapper.ParametersMapper;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.ParametersService;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 商品参数业务层实现
 *
 * @author pikachu
 * @since 2020-03-02 16:18:56
 */
@Service
public class ParametersServiceImpl extends ServiceImpl<ParametersMapper, Parameters> implements ParametersService {


    @Autowired
    private GoodsService goodsService;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private RocketMQTemplate rocketMQTemplate;

    /**
     * 更新参数组信息
     *
     * @param parameters 参数组信息
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateParameter(Parameters parameters) {
        Parameters origin = this.getById(parameters.getId());
        if (origin == null) {
            throw new ServiceException(ResultCode.CATEGORY_NOT_EXIST);
        }

        List<String> goodsIds = new ArrayList<>();
        LambdaQueryWrapper<Goods> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.select(Goods::getId, Goods::getParams);
        queryWrapper.like(Goods::getParams, parameters.getGroupId());
        List<Map<String, Object>> goodsList = this.goodsService.listMaps(queryWrapper);

        if (!goodsList.isEmpty()) {
            for (Map<String, Object> goods : goodsList) {
                String params = (String) goods.get("params");
                List<GoodsParamsDTO> goodsParamsDTOS = JSONUtil.toList(params, GoodsParamsDTO.class);
                List<GoodsParamsDTO> goodsParamsDTOList = goodsParamsDTOS.stream().filter(i -> i.getGroupId() != null && i.getGroupId().equals(parameters.getGroupId())).collect(Collectors.toList());
                this.setGoodsItemDTOList(goodsParamsDTOList, parameters);
                this.goodsService.updateGoodsParams(goods.get("id").toString(), JSONUtil.toJsonStr(goodsParamsDTOS));
                goodsIds.add(goods.get("id").toString());
            }

            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.UPDATE_GOODS_INDEX.name();
            //发送mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(goodsIds), RocketmqSendCallbackBuilder.commonCallback());
        }
        return this.updateById(parameters);
    }

    /**
     * 更新商品参数信息
     *
     * @param goodsParamsDTOList 商品参数项列表
     * @param parameters         参数信息
     */
    private void setGoodsItemDTOList(List<GoodsParamsDTO> goodsParamsDTOList, Parameters parameters) {
        for (GoodsParamsDTO goodsParamsDTO : goodsParamsDTOList) {
            List<GoodsParamsItemDTO> goodsParamsItemDTOList = goodsParamsDTO.getGoodsParamsItemDTOList().stream().filter(i -> i.getParamId() != null && i.getParamId().equals(parameters.getId())).collect(Collectors.toList());
            for (GoodsParamsItemDTO goodsParamsItemDTO : goodsParamsItemDTOList) {
                this.setGoodsItemDTO(goodsParamsItemDTO, parameters);
            }
        }
    }

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