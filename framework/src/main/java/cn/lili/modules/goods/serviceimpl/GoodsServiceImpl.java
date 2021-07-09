package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.GoodsTagsEnum;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dos.Parameters;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.entity.vos.ParameterGroupVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.entity.enums.EvaluationGradeEnum;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 商品业务层实现
 *
 * @author pikachu
 * @date 2020-02-23 15:18:56
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class GoodsServiceImpl extends ServiceImpl<GoodsMapper, Goods> implements GoodsService {


    /**
     * 分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;
    /**
     * 商品相册
     */
    @Autowired
    private GoodsGalleryService goodsGalleryService;
    /**
     * 商品规格
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreService storeService;
    /**
     * 会员评价
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;
    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * 分类-参数
     */
    @Autowired
    private CategoryParameterGroupService categoryParameterGroupService;


    @Override
    public void underStoreGoods(String storeId) {
        //获取商品ID列表
        List<String> list = this.baseMapper.getGoodsIdByStoreId(storeId);
        //下架店铺下的商品
        updateGoodsMarketAble(list, GoodsStatusEnum.DOWN, "店铺关闭");
    }

    @Override
    public final Integer getGoodsCountByCategory(String categoryId) {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.like("category_path", categoryId);
        queryWrapper.eq("delete_flag", false);
        return this.count(queryWrapper);
    }

    @Override
    public void addGoods(GoodsOperationDTO goodsOperationDTO) {
        Goods goods = new Goods(goodsOperationDTO);
        //检查商品
        this.checkGoods(goods);
        //向goods加入图片
        this.setGoodsGalleryParam(goodsOperationDTO.getGoodsGalleryList().get(0), goods);
        //添加商品参数
        if (goodsOperationDTO.getGoodsParamsDTOList() != null && !goodsOperationDTO.getGoodsParamsDTOList().isEmpty()) {
            //检测商品参数是否合法
            //this.checkGoodsParams(goodsOperationDTO.getGoodsParamsDTOList(), goodsOperationDTO.getCategoryPath().substring(goodsOperationDTO.getCategoryPath().lastIndexOf(",") + 1));
            //给商品参数填充值
            goods.setParams(JSONUtil.toJsonStr(goodsOperationDTO.getGoodsParamsDTOList()));
        }
        //添加商品
        this.save(goods);
        //添加商品sku信息
        this.goodsSkuService.add(goodsOperationDTO.getSkuList(), goods);
        //添加相册
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.goodsGalleryService.add(goodsOperationDTO.getGoodsGalleryList(), goods.getId());
        }
    }


    @Override
    public void editGoods(GoodsOperationDTO goodsOperationDTO, String goodsId) {
        Goods goods = new Goods(goodsOperationDTO);
        goods.setId(goodsId);
        //检查商品信息
        this.checkGoods(goods);
        //向goods加入图片
        this.setGoodsGalleryParam(goodsOperationDTO.getGoodsGalleryList().get(0), goods);
        //添加商品参数
        if (goodsOperationDTO.getGoodsParamsDTOList() != null && !goodsOperationDTO.getGoodsParamsDTOList().isEmpty()) {
            goods.setParams(JSONUtil.toJsonStr(goodsOperationDTO.getGoodsParamsDTOList()));
        }
        //修改商品
        this.updateById(goods);
        //修改商品sku信息
        this.goodsSkuService.update(goodsOperationDTO.getSkuList(), goods, goodsOperationDTO.getRegeneratorSkuFlag());
        //添加相册
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.goodsGalleryService.add(goodsOperationDTO.getGoodsGalleryList(), goods.getId());
        }

    }

    @Override
    public GoodsVO getGoodsVO(String goodsId) {
        //查询商品信息
        Goods goods = this.getById(goodsId);
        if (goods == null) {
            log.error("商品ID为" + goodsId + "的商品不存在");
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        GoodsVO goodsVO = new GoodsVO();
        //赋值
        BeanUtils.copyProperties(goods, goodsVO);
        //商品id
        goodsVO.setId(goods.getId());
        //商品相册赋值
        List<String> images = new ArrayList<>();
        List<GoodsGallery> galleryList = goodsGalleryService.goodsGalleryList(goodsId);
        for (GoodsGallery goodsGallery : galleryList) {
            images.add(goodsGallery.getOriginal());
        }
        goodsVO.setGoodsGalleryList(images);
        //商品sku赋值
        List<GoodsSkuVO> goodsListByGoodsId = goodsSkuService.getGoodsListByGoodsId(goodsId);
        if (goodsListByGoodsId != null && !goodsListByGoodsId.isEmpty()) {
            goodsVO.setSkuList(goodsListByGoodsId);
        }
        //商品分类名称赋值
        List<String> categoryName = new ArrayList<>();
        String categoryPath = goods.getCategoryPath();
        String[] strArray = categoryPath.split(",");
        List<Category> categories = categoryService.listByIds(Arrays.asList(strArray));
        for (Category category : categories) {
            categoryName.add(category.getName());
        }
        goodsVO.setCategoryName(categoryName);

        //参数非空则填写参数
        if (StringUtils.isNotEmpty(goods.getParams())) {
            goodsVO.setGoodsParamsDTOList(JSONUtil.toList(goods.getParams(), GoodsParamsDTO.class));
        }

        return goodsVO;
    }

    @Override
    public IPage<Goods> queryByParams(GoodsSearchParams goodsSearchParams) {
        return this.page(PageUtil.initPage(goodsSearchParams), goodsSearchParams.queryWrapper());
    }

    @Override
    public boolean auditGoods(List<String> goodsIds, GoodsAuthEnum goodsAuthEnum) {
        boolean result = false;
        for (String goodsId : goodsIds) {
            Goods goods = this.checkExist(goodsId);
            goods.setIsAuth(goodsAuthEnum.name());
            result = this.updateById(goods);
            goodsSkuService.updateGoodsSkuStatus(goods);
            //商品审核消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_AUDIT.name();
            //发送mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(goods.getStoreId()), RocketmqSendCallbackBuilder.commonCallback());
        }
        return result;
    }

    @Override
    public Integer goodsNum(GoodsStatusEnum goodsStatusEnum, GoodsAuthEnum goodsAuthEnum) {
        LambdaQueryWrapper<Goods> queryWrapper = Wrappers.lambdaQuery();

        queryWrapper.eq(Goods::getDeleteFlag, false);

        if (goodsStatusEnum != null) {
            queryWrapper.eq(Goods::getMarketEnable, goodsStatusEnum.name());
        }
        if (goodsAuthEnum != null) {
            queryWrapper.eq(Goods::getIsAuth, goodsAuthEnum.name());
        }
        queryWrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                Goods::getStoreId, UserContext.getCurrentUser().getStoreId());

        return this.count(queryWrapper);
    }

    @Override
    public Integer todayUpperNum() {
        LambdaQueryWrapper<Goods> queryWrapper = Wrappers.lambdaQuery();
        queryWrapper.eq(Goods::getMarketEnable, GoodsStatusEnum.UPPER.name());
        queryWrapper.gt(Goods::getCreateTime, DateUtil.beginOfDay(new DateTime()));
        return this.count(queryWrapper);
    }

    @Override
    public Boolean updateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        LambdaUpdateWrapper<Goods> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.set(Goods::getMarketEnable, goodsStatusEnum.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        updateWrapper.in(Goods::getId, goodsIds);
        this.update(updateWrapper);

        //修改规格商品
        List<Goods> goodsList = this.list(new LambdaQueryWrapper<Goods>().in(Goods::getId, goodsIds));
        for (Goods goods : goodsList) {
            goodsSkuService.updateGoodsSkuStatus(goods);
        }
        return true;

    }

    @Override
    public Boolean deleteGoods(List<String> goodsIds) {

        LambdaUpdateWrapper<Goods> updateWrapper = Wrappers.lambdaUpdate();
        updateWrapper.set(Goods::getMarketEnable, GoodsStatusEnum.DOWN.name());
        updateWrapper.set(Goods::getDeleteFlag, true);
        updateWrapper.in(Goods::getId, goodsIds);
        this.update(updateWrapper);

        //修改规格商品
        List<Goods> goodsList = this.list(new LambdaQueryWrapper<Goods>().in(Goods::getId, goodsIds));
        for (Goods goods : goodsList) {
            //修改SKU状态
            goodsSkuService.updateGoodsSkuStatus(goods);
            //商品删除消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_DELETE.name();
            //发送mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(goods), RocketmqSendCallbackBuilder.commonCallback());
        }

        return true;
    }

    @Override
    public Boolean freight(List<String> goodsIds, String templateId) {
        LambdaUpdateWrapper<Goods> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.set(Goods::getTemplateId, templateId);
        lambdaUpdateWrapper.in(Goods::getId, goodsIds);
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public void updateStock(String goodsId, Integer quantity) {
        LambdaUpdateWrapper<Goods> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.set(Goods::getQuantity, quantity);
        lambdaUpdateWrapper.eq(Goods::getId, goodsId);
        this.update(lambdaUpdateWrapper);
    }

    @Override
    public void updateGoodsCommentNum(String goodsId) {

        //获取商品信息
        Goods goods = this.getById(goodsId);
        //修改商品评价数量
        goods.setCommentNum(goods.getCommentNum() + 1);

        //修改商品好评率
        LambdaQueryWrapper<MemberEvaluation> goodEvaluationQueryWrapper = new LambdaQueryWrapper<>();
        goodEvaluationQueryWrapper.eq(MemberEvaluation::getId, goodsId);
        goodEvaluationQueryWrapper.eq(MemberEvaluation::getGrade, EvaluationGradeEnum.GOOD.name());
        //好评数量
        int highPraiseNum = memberEvaluationService.count(goodEvaluationQueryWrapper);
        //好评率
        double grade = NumberUtil.mul(NumberUtil.div(highPraiseNum, goods.getCommentNum().doubleValue(), 2), 100);
        goods.setGrade(grade);
        this.updateById(goods);
    }

    /**
     * 添加商品默认图片
     *
     * @param origin 图片
     * @param goods  商品
     */
    private void setGoodsGalleryParam(String origin, Goods goods) {
        GoodsGallery goodsGallery = goodsGalleryService.getGoodsGallery(origin);
        goods.setOriginal(goodsGallery.getOriginal());
        goods.setSmall(goodsGallery.getSmall());
        goods.setThumbnail(goodsGallery.getThumbnail());
    }

    /**
     * 检测商品参数是否非法传递
     *
     * @param goodsParamsDTOS 商品参数
     * @param categoryId      分类id
     */
    private void checkGoodsParams(List<GoodsParamsDTO> goodsParamsDTOS, String categoryId) {
        //根据绑定的分了id查询出参数信息
        List<ParameterGroupVO> parameterGroupVOS = categoryParameterGroupService.getCategoryParams(categoryId);
        if (parameterGroupVOS.size() > 0) {
            //绑定分类的参数集合
            List<Parameters> parametersList = new ArrayList<>();
            //循环分类绑定的参数信息 把它整理到新的分类参数集合中 用于最后的参数信息对比
            for (ParameterGroupVO parameterGroupVO : parameterGroupVOS) {
                List<Parameters> parameters = parameterGroupVO.getParams();
                for (Parameters param : parameters) {
                    parametersList.add(param);
                }
            }
            List<GoodsParamsItemDTO> goodsOperationParamList = new ArrayList<>();
            //循环添加商品传递的参数信息 把它整理到新的分类参数集合中 用于最后的参数信息对比
            for (GoodsParamsDTO goodsParamsDTO : goodsParamsDTOS) {
                List<GoodsParamsItemDTO> goodsParamsItemDTOS = goodsParamsDTO.getGoodsParamsItemDTOList();
                for (GoodsParamsItemDTO goodsParamsItemDTO : goodsParamsItemDTOS) {
                    goodsOperationParamList.add(goodsParamsItemDTO);
                }
            }
            //两个参数集合进行对比
            for (Parameters parameters : parametersList) {
                for (GoodsParamsItemDTO goodsParamsItemDTO : goodsOperationParamList) {
                    if (parameters.getId().equals(goodsParamsItemDTO.getParamId())) {
                        //校验是否可以索引参数是否正确
                        if (!parameters.getIsIndex().equals(goodsParamsItemDTO.getIsIndex())) {
                            throw new ServiceException(ResultCode.GOODS_PARAMS_ERROR);
                        }
                        //校验是否必填参数是否正确
                        if (!parameters.getRequired().equals(goodsParamsItemDTO.getRequired())) {
                            throw new ServiceException(ResultCode.GOODS_PARAMS_ERROR);
                        }
                    }
                }
            }

        }
    }

    /**
     * 检查商品信息
     * 如果商品是虚拟商品则无需配置配送模板
     * 如果商品是实物商品需要配置配送模板
     * 判断商品是否存在
     * 判断商品是否需要审核
     * 判断当前用户是否为店铺
     *
     * @param goods 商品
     */
    private void checkGoods(Goods goods) {
        //判断商品类型
        switch (goods.getGoodsType()) {
            case "PHYSICAL_GOODS":
                if ("0".equals(goods.getTemplateId())) {
                    throw new ServiceException(ResultCode.PHYSICAL_GOODS_NEED_TEMP);
                }
                break;
            case "VIRTUAL_GOODS":
                if (!"0".equals(goods.getTemplateId())) {
                    throw new ServiceException(ResultCode.VIRTUAL_GOODS_NOT_NEED_TEMP);
                }
                break;
            default:
                throw new ServiceException(ResultCode.GOODS_TYPE_ERROR);
        }
        //检查商品是否存在--修改商品时使用
        if (goods.getId() != null) {
            this.checkExist(goods.getId());
        } else {
            //评论次数
            goods.setCommentNum(0);
            //购买次数
            goods.setBuyCount(0);
            //购买次数
            goods.setQuantity(0);
            //商品评分
            goods.setGrade(100.0);
        }

        //获取商品系统配置决定是否审核
        Setting setting = settingService.get(SettingEnum.GOODS_SETTING.name());
        GoodsSetting goodsSetting = JSONUtil.toBean(setting.getSettingValue(), GoodsSetting.class);
        //是否需要审核
        goods.setIsAuth(Boolean.TRUE.equals(goodsSetting.getGoodsCheck()) ? GoodsAuthEnum.TOBEAUDITED.name() : GoodsAuthEnum.PASS.name());
        //判断当前用户是否为店铺
        if (UserContext.getCurrentUser().getRole().equals(UserEnums.STORE)) {
            StoreVO storeDetail = this.storeService.getStoreDetail();
            if (storeDetail.getSelfOperated() != null) {
                goods.setSelfOperated(storeDetail.getSelfOperated());
            }
            goods.setStoreId(storeDetail.getId());
            goods.setStoreName(storeDetail.getStoreName());
            goods.setSelfOperated(storeDetail.getSelfOperated());
        } else {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
    }

    /**
     * 判断商品是否存在
     *
     * @param goodsId
     * @return
     */
    private Goods checkExist(String goodsId) {
        Goods goods = getById(goodsId);
        if (goods == null) {
            log.error("商品ID为" + goodsId + "的商品不存在");
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return goods;
    }

}