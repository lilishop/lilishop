package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.map.MapUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.*;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsNumVO;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.member.entity.dto.EvaluationQueryParams;
import cn.lili.modules.member.entity.enums.EvaluationGradeEnum;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.search.utils.EsIndexUtil;
import cn.lili.modules.store.entity.dos.FreightTemplate;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品业务层实现
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Service
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


    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;
    @Autowired
    private FreightTemplateService freightTemplateService;

    @Autowired
    private WholesaleService wholesaleService;

    @Autowired
    private Cache<GoodsVO> cache;

    @Override
    public List<Goods> getByBrandIds(List<String> brandIds) {
        LambdaQueryWrapper<Goods> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.in(Goods::getBrandId, brandIds);
        return list(lambdaQueryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void underStoreGoods(String storeId) {
        //获取商品ID列表
        List<String> list = this.baseMapper.getGoodsIdByStoreId(storeId);
        //下架店铺下的商品
        this.updateGoodsMarketAbleByStoreId(storeId, GoodsStatusEnum.DOWN, "店铺关闭");

        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("下架商品",
                rocketmqCustomProperties.getGoodsTopic(), GoodsTagsEnum.DOWN.name(), JSONUtil.toJsonStr(list)));

    }

    /**
     * 更新商品参数
     *
     * @param goodsId 商品id
     * @param params  商品参数
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsParams(String goodsId, String params) {
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Goods::getId, goodsId);
        updateWrapper.set(Goods::getParams, params);
        this.update(updateWrapper);
    }

    @Override
    public final long getGoodsCountByCategory(String categoryId) {
        QueryWrapper<Goods> queryWrapper = Wrappers.query();
        queryWrapper.like("category_path", categoryId);
        queryWrapper.eq("delete_flag", false);
        return this.count(queryWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "添加商品", customerLog = "'新增商品名称:['+#goodsOperationDTO.goodsName+']'")
    public void addGoods(GoodsOperationDTO goodsOperationDTO) {
        Goods goods = new Goods(goodsOperationDTO);
        //检查商品
        this.checkGoods(goods);
        //向goods加入图片
        if (goodsOperationDTO.getGoodsGalleryList().size() > 0) {
            this.setGoodsGalleryParam(goodsOperationDTO.getGoodsGalleryList().get(0), goods);
        }
        //添加商品参数
        if (goodsOperationDTO.getGoodsParamsDTOList() != null && !goodsOperationDTO.getGoodsParamsDTOList().isEmpty()) {
            //给商品参数填充值
            goods.setParams(JSONUtil.toJsonStr(goodsOperationDTO.getGoodsParamsDTOList()));
        }
        //添加商品
        this.save(goods);
        //添加商品sku信息
        this.goodsSkuService.add(goods, goodsOperationDTO);
        //添加相册
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.goodsGalleryService.add(goodsOperationDTO.getGoodsGalleryList(), goods.getId());
        }
        this.generateEs(goods);
    }


    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "修改商品", customerLog = "'操作的商品ID:['+#goodsId+']'")
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
        this.goodsSkuService.update(goods, goodsOperationDTO);
        //添加相册
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.goodsGalleryService.add(goodsOperationDTO.getGoodsGalleryList(), goods.getId());
        }
        if (GoodsAuthEnum.TOBEAUDITED.name().equals(goods.getAuthFlag())) {
            this.deleteEsGoods(Collections.singletonList(goodsId));
        }
        cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
        this.generateEs(goods);
    }


    @Override
    public GoodsVO getGoodsVO(String goodsId) {
        //缓存获取，如果没有则读取缓存
        GoodsVO goodsVO = cache.get(CachePrefix.GOODS.getPrefix() + goodsId);
        if (goodsVO != null) {
            return goodsVO;
        }
        //查询商品信息
        Goods goods = this.getById(goodsId);
        if (goods == null) {
            log.error("商品ID为" + goodsId + "的商品不存在");
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        goodsVO = new GoodsVO();
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
        if (CharSequenceUtil.isNotEmpty(goods.getParams())) {
            goodsVO.setGoodsParamsDTOList(JSONUtil.toList(goods.getParams(), GoodsParamsDTO.class));
        }

        List<Wholesale> wholesaleList = wholesaleService.findByGoodsId(goodsId);
        if (CollUtil.isNotEmpty(wholesaleList)) {
            goodsVO.setWholesaleList(wholesaleList);
        }

        cache.put(CachePrefix.GOODS.getPrefix() + goodsId, goodsVO,7200L);
        return goodsVO;
    }

    @Override
    public IPage<Goods> queryByParams(GoodsSearchParams goodsSearchParams) {
        return this.page(PageUtil.initPage(goodsSearchParams), goodsSearchParams.queryWrapper());
    }

    @Override
    public GoodsNumVO getGoodsNumVO(GoodsSearchParams goodsSearchParams) {
        GoodsNumVO goodsNumVO = new GoodsNumVO();
        
        // 获取基础查询条件
        QueryWrapper<Goods> baseWrapper = goodsSearchParams.queryWrapper();
        
        // 使用聚合查询一次性获取所有状态的商品数量
        List<Map<String, Object>> results = this.baseMapper.selectMaps(
            baseWrapper.select(
                "COUNT(CASE WHEN auth_flag = 'PASS' AND market_enable = 'UPPER' THEN 1 END) as upperGoodsNum",
                "COUNT(CASE WHEN auth_flag = 'PASS' AND market_enable = 'DOWN' THEN 1 END) as downGoodsNum",
                "COUNT(CASE WHEN auth_flag = 'TOBEAUDITED' THEN 1 END) as auditGoodsNum",
                "COUNT(CASE WHEN auth_flag = 'REFUSE' THEN 1 END) as refuseGoodsNum"
            )
        );
        
        if (!results.isEmpty()) {
            Map<String, Object> result = results.get(0);
            goodsNumVO.setUpperGoodsNum(((Number) result.get("upperGoodsNum")).intValue());
            goodsNumVO.setDownGoodsNum(((Number) result.get("downGoodsNum")).intValue());
            goodsNumVO.setAuditGoodsNum(((Number) result.get("auditGoodsNum")).intValue());
            goodsNumVO.setRefuseGoodsNum(((Number) result.get("refuseGoodsNum")).intValue());
        } else {
            // 如果没有结果，设置默认值为0
            goodsNumVO.setUpperGoodsNum(0);
            goodsNumVO.setDownGoodsNum(0);
            goodsNumVO.setAuditGoodsNum(0);
            goodsNumVO.setRefuseGoodsNum(0);
        }
        
        return goodsNumVO;
    }

    /**
     * 商品查询
     *
     * @param goodsSearchParams 查询参数
     * @return 商品信息
     */
    @Override
    public List<Goods> queryListByParams(GoodsSearchParams goodsSearchParams) {
        return this.list(goodsSearchParams.queryWrapper());
    }

    @Override
    @SystemLogPoint(description = "审核商品", customerLog = "'操作的商品ID:['+#goodsIds+']，操作后商品状态:['+#goodsAuthEnum+']'")
    @Transactional(rollbackFor = Exception.class)
    public boolean auditGoods(List<String> goodsIds, GoodsAuthEnum goodsAuthEnum) {
        List<String> goodsCacheKeys = new ArrayList<>();
        boolean result = false;
        for (String goodsId : goodsIds) {
            Goods goods = this.checkExist(goodsId);
            goods.setAuthFlag(goodsAuthEnum.name());
            result = this.updateById(goods);
            goodsSkuService.updateGoodsSkuStatus(goods);
            //删除之前的缓存
            goodsCacheKeys.add(CachePrefix.GOODS.getPrefix() + goodsId);
            //商品审核消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_AUDIT.name();
            //发送mq消息
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(goods), RocketmqSendCallbackBuilder.commonCallback());
        }
        cache.multiDel(goodsCacheKeys);
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "商品状态操作", customerLog = "'操作类型:['+#goodsStatusEnum+']，操作对象:['+#goodsIds+']，操作原因:['+#underReason+']'")
    public Boolean updateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        boolean result;

        //如果商品为空，直接返回
        if (goodsIds == null || goodsIds.isEmpty()) {
            return true;
        }

        LambdaUpdateWrapper<Goods> updateWrapper = this.getUpdateWrapperByStoreAuthority();
        updateWrapper.set(Goods::getMarketEnable, goodsStatusEnum.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        updateWrapper.in(Goods::getId, goodsIds);
        result = this.update(updateWrapper);

        //修改规格商品
        LambdaQueryWrapper<Goods> queryWrapper = this.getQueryWrapperByStoreAuthority();
        queryWrapper.in(Goods::getId, goodsIds);
        List<Goods> goodsList = this.list(queryWrapper);
        this.updateGoodsStatus(goodsIds, goodsStatusEnum, goodsList);
        return result;
    }

    /**
     * 更新商品上架状态状态
     *
     * @param storeId         店铺ID
     * @param goodsStatusEnum 更新的商品状态
     * @param underReason     下架原因
     * @return 更新结果
     */
    @Override
    @SystemLogPoint(description = "店铺关闭下架商品", customerLog = "'操作类型:['+#goodsStatusEnum+']，操作对象:['+#storeId+']，操作原因:['+#underReason+']'")
    public Boolean updateGoodsMarketAbleByStoreId(String storeId, GoodsStatusEnum goodsStatusEnum, String underReason) {


        LambdaUpdateWrapper<Goods> updateWrapper = this.getUpdateWrapperByStoreAuthority();
        updateWrapper.set(Goods::getMarketEnable, goodsStatusEnum.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        updateWrapper.eq(Goods::getStoreId, storeId);
        boolean result = this.update(updateWrapper);

        //修改规格商品
        this.goodsSkuService.updateGoodsSkuStatusByStoreId(storeId, goodsStatusEnum.name(), null);
        return result;
    }

    @Override
    @SystemLogPoint(description = "管理员关闭下架商品", customerLog = "'操作类型:['+#goodsStatusEnum+']，操作对象:['+#goodsIds+']，操作原因:['+#underReason+']'")
    @Transactional(rollbackFor = Exception.class)

    public Boolean managerUpdateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        boolean result;

        //如果商品为空，直接返回
        if (goodsIds == null || goodsIds.isEmpty()) {
            return true;
        }

        //检测管理员权限
        this.checkManagerAuthority();

        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.set(Goods::getMarketEnable, goodsStatusEnum.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        updateWrapper.in(Goods::getId, goodsIds);
        result = this.update(updateWrapper);

        //修改规格商品
        LambdaQueryWrapper<Goods> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.in(Goods::getId, goodsIds);
        List<Goods> goodsList = this.list(queryWrapper);
        this.updateGoodsStatus(goodsIds, goodsStatusEnum, goodsList);
        return result;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "删除商品", customerLog = "'操作对象:['+#goodsIds+']'")
    public Boolean deleteGoods(List<String> goodsIds) {

        LambdaUpdateWrapper<Goods> updateWrapper = this.getUpdateWrapperByStoreAuthority();
        updateWrapper.set(Goods::getMarketEnable, GoodsStatusEnum.DOWN.name());
        updateWrapper.set(Goods::getDeleteFlag, true);
        updateWrapper.in(Goods::getId, goodsIds);
        this.update(updateWrapper);

        //修改规格商品
        LambdaQueryWrapper<Goods> queryWrapper = this.getQueryWrapperByStoreAuthority();
        queryWrapper.in(Goods::getId, goodsIds);
        List<Goods> goodsList = this.list(queryWrapper);
        List<String> goodsCacheKeys = new ArrayList<>();
        for (Goods goods : goodsList) {
            //修改SKU状态
            goodsSkuService.updateGoodsSkuStatus(goods);
            goodsCacheKeys.add(CachePrefix.GOODS.getPrefix() + goods.getId());
        }
        //删除缓存
        cache.multiDel(goodsCacheKeys);
        this.deleteEsGoods(goodsIds);
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean freight(List<String> goodsIds, String templateId) {

        AuthUser authUser = this.checkStoreAuthority();

        FreightTemplate freightTemplate = freightTemplateService.getById(templateId);
        if (freightTemplate == null) {
            throw new ServiceException(ResultCode.FREIGHT_TEMPLATE_NOT_EXIST);
        }
        if (authUser != null && !freightTemplate.getStoreId().equals(authUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        LambdaUpdateWrapper<Goods> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.set(Goods::getTemplateId, templateId);
        lambdaUpdateWrapper.in(Goods::getId, goodsIds);
        List<String> goodsCache = goodsIds.stream().map(item -> CachePrefix.GOODS.getPrefix() + item).collect(Collectors.toList());
        cache.multiDel(goodsCache);
        goodsSkuService.freight(goodsIds, templateId);
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    @SystemLogPoint(description = "同步商品库存", customerLog = "'同步商品商品ID的库存:['+#goodsId+']'")
    public void updateStock(String goodsId) {
        LambdaUpdateWrapper<Goods> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        Integer stock = goodsSkuService.getGoodsStock(goodsId);
        lambdaUpdateWrapper.set(Goods::getQuantity, stock);
        lambdaUpdateWrapper.eq(Goods::getId, goodsId);
        cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
        this.update(lambdaUpdateWrapper);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsCommentNum(String goodsId, String skuId) {
        GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (goodsSku == null) {
            return;
        }

        //获取商品信息
        Goods goods = this.getById(goodsId);

        if (goods == null) {
            return;
        }

        //修改商品评价数量
        long commentNum = memberEvaluationService.getEvaluationCount(EvaluationQueryParams.builder().goodsId(goodsId).status("OPEN").build());
        goods.setCommentNum((int) (commentNum));

        //好评数量
        long highPraiseNum = memberEvaluationService.getEvaluationCount(EvaluationQueryParams.builder().goodsId(goodsId).status("OPEN").grade(EvaluationGradeEnum.GOOD.name()).build());
        //好评率
        double grade = NumberUtil.mul(NumberUtil.div(highPraiseNum, goods.getCommentNum().doubleValue(), 2), 100);
        goods.setGrade(grade);
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Goods::getId, goodsId);
        updateWrapper.set(Goods::getCommentNum, goods.getCommentNum());
        updateWrapper.set(Goods::getGrade, goods.getGrade());
        this.update(updateWrapper);

        cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);


        // 修改商品sku评价数量
        this.goodsSkuService.updateGoodsSkuGrade(goodsId, grade, goods.getCommentNum());

        Map<String, Object> updateIndexFieldsMap = EsIndexUtil.getUpdateIndexFieldsMap(MapUtil.builder(new HashMap<String, Object>()).put("goodsId", goodsId).build(), MapUtil.builder(new HashMap<String, Object>()).put("commentNum", goods.getCommentNum()).put("highPraiseNum", highPraiseNum).put("grade", grade).build());
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("更新商品索引信息", rocketmqCustomProperties.getGoodsTopic(), GoodsTagsEnum.UPDATE_GOODS_INDEX_FIELD.name(), JSONUtil.toJsonStr(updateIndexFieldsMap)));
    }

    /**
     * 更新商品的购买数量
     *
     * @param goodsId  商品ID
     * @param buyCount 购买数量
     */
    @Override
    public void updateGoodsBuyCount(String goodsId, int buyCount) {
        this.update(new LambdaUpdateWrapper<Goods>()
                .eq(Goods::getId, goodsId)
                .set(Goods::getBuyCount, buyCount));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStoreDetail(Store store) {
        UpdateWrapper updateWrapper = new UpdateWrapper<>()
                .eq("store_id", store.getId())
                .set("store_name", store.getStoreName())
                .set("self_operated", store.getSelfOperated());
        this.update(updateWrapper);
        goodsSkuService.update(updateWrapper);
    }

    @Override
    public long countStoreGoodsNum(String storeId) {
        return this.count(
                new LambdaQueryWrapper<Goods>()
                        .eq(Goods::getStoreId, storeId)
                        .eq(Goods::getDeleteFlag, Boolean.FALSE)
                        .eq(Goods::getAuthFlag, GoodsAuthEnum.PASS.name())
                        .eq(Goods::getMarketEnable, GoodsStatusEnum.UPPER.name()));
    }

    @Override
    public void categoryGoodsName(String categoryId) {
        //获取分类下的商品
        List<Goods> list = this.list(new LambdaQueryWrapper<Goods>().like(Goods::getCategoryPath, categoryId));
        list.parallelStream().forEach(goods -> {
            //移除redis中商品缓存
            cache.remove(CachePrefix.GOODS.getPrefix() + goods.getId());
        });
    }

    @Override
    public void addGoodsCommentNum(Integer commentNum, String goodsId) {
        this.baseMapper.addGoodsCommentNum(commentNum, goodsId);
    }

    /**
     * 更新商品状态
     *
     * @param goodsIds        商品ID
     * @param goodsStatusEnum 商品状态
     * @param goodsList       商品列表
     */
    private void updateGoodsStatus(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, List<Goods> goodsList) {
        List<String> goodsCacheKeys = new ArrayList<>();
        for (Goods goods : goodsList) {
            goodsCacheKeys.add(CachePrefix.GOODS.getPrefix() + goods.getId());
            goodsSkuService.updateGoodsSkuStatus(goods);
        }
        cache.multiDel(goodsCacheKeys);

        if (GoodsStatusEnum.DOWN.equals(goodsStatusEnum)) {
            this.deleteEsGoods(goodsIds);
        } else {
            this.updateEsGoods(goodsIds);
        }


        //下架商品发送消息
        if (goodsStatusEnum.equals(GoodsStatusEnum.DOWN)) {
            applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("下架商品",
                    rocketmqCustomProperties.getGoodsTopic(), GoodsTagsEnum.DOWN.name(), JSONUtil.toJsonStr(goodsIds)));
        }
    }

    /**
     * 发送生成ES商品索引
     *
     * @param goods 商品信息
     */
    @Transactional
    public void generateEs(Goods goods) {
        // 不生成没有审核通过且没有上架的商品
        if (!GoodsStatusEnum.UPPER.name().equals(goods.getMarketEnable()) || !GoodsAuthEnum.PASS.name().equals(goods.getAuthFlag())) {
            return;
        }
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("生成商品", rocketmqCustomProperties.getGoodsTopic(),
                GoodsTagsEnum.GENERATOR_GOODS_INDEX.name(), goods.getId()));
    }

    /**
     * 发送生成ES商品索引
     *
     * @param goodsIds 商品id
     */
    @Transactional
    public void updateEsGoods(List<String> goodsIds) {
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("更新商品", rocketmqCustomProperties.getGoodsTopic(),
                GoodsTagsEnum.UPDATE_GOODS_INDEX.name(), goodsIds));
    }

    /**
     * 发送删除es索引的信息
     *
     * @param goodsIds 商品id
     */
    @Transactional
    public void deleteEsGoods(List<String> goodsIds) {
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("删除商品", rocketmqCustomProperties.getGoodsTopic(),
                GoodsTagsEnum.GOODS_DELETE.name(), JSONUtil.toJsonStr(goodsIds)));
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
                    goods.setTemplateId("0");
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
        goods.setAuthFlag(Boolean.TRUE.equals(goodsSetting.getGoodsCheck()) ? GoodsAuthEnum.TOBEAUDITED.name() : GoodsAuthEnum.PASS.name());
        //判断当前用户是否为店铺
        if (Objects.requireNonNull(UserContext.getCurrentUser()).getRole().equals(UserEnums.STORE)) {
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
     * @param goodsId 商品id
     * @return 商品信息
     */
    private Goods checkExist(String goodsId) {
        Goods goods = getById(goodsId);
        if (goods == null) {
            log.error("商品ID为" + goodsId + "的商品不存在");
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return goods;
    }


    /**
     * 获取UpdateWrapper（检查用户越权）
     *
     * @return updateWrapper
     */
    private LambdaUpdateWrapper<Goods> getUpdateWrapperByStoreAuthority() {
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        AuthUser authUser = this.checkStoreAuthority();
        if (authUser != null) {
            updateWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        }
        return updateWrapper;
    }


    /**
     * 检查当前登录的店铺
     *
     * @return 当前登录的店铺
     */
    private AuthUser checkStoreAuthority() {
        AuthUser currentUser = UserContext.getCurrentUser();
        //如果当前会员不为空，且为店铺角色
        if (currentUser != null && (currentUser.getRole().equals(UserEnums.STORE) && currentUser.getStoreId() != null)) {
            return currentUser;
        }
        return null;
    }

    /**
     * 检查当前登录的店铺
     *
     * @return 当前登录的店铺
     */
    private AuthUser checkManagerAuthority() {
        AuthUser currentUser = UserContext.getCurrentUser();
        //如果当前会员不为空，且为店铺角色
        if (currentUser != null && (currentUser.getRole().equals(UserEnums.MANAGER))) {
            return currentUser;
        } else {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
    }

    /**
     * 获取QueryWrapper（检查用户越权）
     *
     * @return queryWrapper
     */
    private LambdaQueryWrapper<Goods> getQueryWrapperByStoreAuthority() {
        LambdaQueryWrapper<Goods> queryWrapper = new LambdaQueryWrapper<>();
        AuthUser authUser = this.checkStoreAuthority();
        if (authUser != null) {
            queryWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        }
        return queryWrapper;
    }

}
