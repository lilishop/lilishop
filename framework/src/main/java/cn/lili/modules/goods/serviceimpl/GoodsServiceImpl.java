package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.sensitive.SensitiveWordsHelper;
import cn.hutool.core.collection.CollUtil;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.store.entity.dos.FreightTemplate;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.modules.appointment.support.AppointmentGoodsMetadataSupport;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsNumVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.*;
import cn.lili.modules.goods.support.GoodsMarketAuditManager;
import cn.lili.modules.goods.support.GoodsVOAssembler;
import cn.lili.modules.goods.support.CardKeyPoolSupport;
import cn.lili.modules.goods.support.GoodsStockAggregateSupport;
import cn.lili.modules.goods.util.GoodsCodeGenerator;
import cn.lili.modules.member.entity.dos.MemberEvaluation;
import cn.lili.modules.member.entity.dto.EvaluationQueryParams;
import cn.lili.modules.member.mapper.MemberEvaluationMapper;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.vos.StoreVO;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.GoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.trigger.interfaces.TimeTrigger;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品业务层实现，商品查询、库存汇总、卡密池及预约发品通过独立组件协作，避免服务相互依赖。
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Service
public class GoodsServiceImpl extends ServiceImpl<GoodsMapper, Goods> implements GoodsService {

    @Autowired
    private SettingService settingService;

    @Autowired
    private GoodsGalleryService goodsGalleryService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private StoreService storeService;

    @Autowired
    private MemberEvaluationMapper memberEvaluationMapper;

    @Autowired
    private CardKeyPoolSupport cardKeyPoolSupport;

    @Autowired
    private AppointmentGoodsMetadataSupport appointmentGoodsMetadataSupport;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private TimeTrigger timeTrigger;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private FreightTemplateService freightTemplateService;

    @Autowired
    private Cache<GoodsVO> cache;

    @Autowired
    private GoodsSearchParamService goodsSearchParamService;

    @Autowired
    private GoodsVOAssembler goodsVOAssembler;

    @Autowired
    private GoodsStockAggregateSupport stockAggregates;

    @Autowired
    private GoodsMarketAuditManager goodsMarketAuditManager;

    private GoodsVOAssembler getGoodsVOAssembler() {
        return this.goodsVOAssembler;
    }

    private GoodsMarketAuditManager getGoodsMarketAuditManager() {
        return this.goodsMarketAuditManager;
    }

    @Override
    public List<Goods> getByBrandIds(List<String> brandIds) {
        return this.list(new LambdaQueryWrapper<Goods>().in(Goods::getBrandId, brandIds));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void underStoreGoods(String storeId) {
        this.updateGoodsMarketAbleByStoreId(storeId, GoodsStatusEnum.DOWN, "店铺关闭");
    }

    @Override
    public long getGoodsCountByCategory(String categoryId) {
        return this.count(new LambdaQueryWrapper<Goods>()
                .like(Goods::getCategoryPath, categoryId)
                .eq(Goods::getDeleteFlag, false));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "添加商品", customerLog = "'新增商品名称:['+#goodsOperationDTO.goodsName+']'")
    public String addGoods(GoodsOperationDTO goodsOperationDTO) {
        filterGoodsSensitiveWords(goodsOperationDTO);
        Goods goods = new Goods(goodsOperationDTO);
        if (CharSequenceUtil.isBlank(goods.getSpuSn())) {
            goods.setSpuSn(GoodsCodeGenerator.spuCode());
        }
        this.checkGoods(goods);
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.setGoodsGalleryParam(goodsOperationDTO.getGoodsGalleryList().get(0), goods);
        }

        int totalStock = 0;
        if (goodsOperationDTO.getSkuList() != null && !goodsOperationDTO.getSkuList().isEmpty()) {
            for (Map<String, Object> sku : goodsOperationDTO.getSkuList()) {
                Object quantityObj = sku.get("quantity");
                if (quantityObj != null) {
                    totalStock += Integer.parseInt(quantityObj.toString());
                }
            }
        }

        if (totalStock == 0) {
            goods.setMarketEnable(GoodsStatusEnum.DOWN.name());
        }

        this.save(goods);
        if (goods.getParams() != null) {
            goodsSearchParamService.syncByGoodsId(goods.getId(), goods.getParams());
        }
        this.goodsSkuService.add(goods, goodsOperationDTO);
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.goodsGalleryService.add(goodsOperationDTO.getGoodsGalleryList(), goods.getId());
        }
        if (GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goods.getGoodsType())) {
            appointmentGoodsMetadataSupport.syncGoodsExtFromPublish(goods.getId(), goodsOperationDTO);
        }
        return goods.getId();
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "修改商品", customerLog = "'操作的商品ID:['+#goodsId+']'")
    public void editGoods(GoodsOperationDTO goodsOperationDTO, String goodsId) {
        filterGoodsSensitiveWords(goodsOperationDTO);
        Goods goods = new Goods(goodsOperationDTO);
        goods.setId(goodsId);
        this.checkGoods(goods);
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.setGoodsGalleryParam(goodsOperationDTO.getGoodsGalleryList().get(0), goods);
        }

        int totalStock = 0;
        if (goodsOperationDTO.getSkuList() != null && !goodsOperationDTO.getSkuList().isEmpty()) {
            for (Map<String, Object> sku : goodsOperationDTO.getSkuList()) {
                Object quantityObj = sku.get("quantity");
                if (quantityObj != null) {
                    totalStock += Integer.parseInt(quantityObj.toString());
                }
            }
        }

        if (totalStock == 0) {
            goods.setMarketEnable(GoodsStatusEnum.DOWN.name());
        }

        //修改商品
        this.updateById(goods);
        LambdaUpdateWrapper<Goods> scheduleClearWrapper = new LambdaUpdateWrapper<>();
        scheduleClearWrapper.eq(Goods::getId, goodsId)
                .set(Goods::getScheduledUpperTime, null)
                .set(Goods::getScheduledUpperReason, null)
                .set(Goods::getScheduledDownTime, null)
                .set(Goods::getScheduledDownReason, null);
        this.update(scheduleClearWrapper);
        if (goods.getParams() != null) {
            goodsSearchParamService.syncByGoodsId(goods.getId(), goods.getParams());
        }
        this.goodsSkuService.update(goods, goodsOperationDTO);
        if (goodsOperationDTO.getGoodsGalleryList() != null && !goodsOperationDTO.getGoodsGalleryList().isEmpty()) {
            this.goodsGalleryService.add(goodsOperationDTO.getGoodsGalleryList(), goods.getId());
        }
        if (GoodsAuthEnum.TOBEAUDITED.name().equals(goods.getAuthFlag())) {
            this.notifyGoodsDelete(Collections.singletonList(goodsId));
        }
        if (GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goods.getGoodsType())) {
            appointmentGoodsMetadataSupport.syncGoodsExtFromPublish(goodsId, goodsOperationDTO);
        }
        cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
    }

    private void filterGoodsSensitiveWords(GoodsOperationDTO goodsOperationDTO) {
        if (goodsOperationDTO == null) {
            return;
        }
        goodsOperationDTO.setGoodsName(SensitiveWordsHelper.filter(SensitiveFilterScene.GOODS, goodsOperationDTO.getGoodsName()));
        goodsOperationDTO.setSellingPoint(SensitiveWordsHelper.filter(SensitiveFilterScene.GOODS, goodsOperationDTO.getSellingPoint()));
        goodsOperationDTO.setIntro(SensitiveWordsHelper.filter(SensitiveFilterScene.GOODS, goodsOperationDTO.getIntro()));
        goodsOperationDTO.setMobileIntro(SensitiveWordsHelper.filter(SensitiveFilterScene.GOODS, goodsOperationDTO.getMobileIntro()));
        if (CollUtil.isNotEmpty(goodsOperationDTO.getGoodsParamsDTOList())) {
            for (GoodsParamsItemDTO goodsParamsItemDTO : goodsOperationDTO.getGoodsParamsDTOList()) {
                goodsParamsItemDTO.setParamName(SensitiveWordsHelper.filter(SensitiveFilterScene.GOODS, goodsParamsItemDTO.getParamName()));
                goodsParamsItemDTO.setParamValue(SensitiveWordsHelper.filter(SensitiveFilterScene.GOODS, goodsParamsItemDTO.getParamValue()));
            }
        }
    }

    @Override
    public GoodsVO getGoodsVO(String goodsId) {
        return getGoodsVOAssembler().getGoodsVO(goodsId);
    }

    @Override
    public IPage<Goods> queryByParams(GoodsSearchParams goodsSearchParams) {
        return this.page(PageUtil.initPage(goodsSearchParams), goodsSearchParams.queryWrapper());
    }

    @Override
    public GoodsNumVO getGoodsNumVO(GoodsSearchParams goodsSearchParams) {
        GoodsNumVO goodsNumVO = new GoodsNumVO();
        QueryWrapper<Goods> baseWrapper = goodsSearchParams.queryWrapper();
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
            goodsNumVO.setUpperGoodsNum(0);
            goodsNumVO.setDownGoodsNum(0);
            goodsNumVO.setAuditGoodsNum(0);
            goodsNumVO.setRefuseGoodsNum(0);
        }
        return goodsNumVO;
    }

    @Override
    public List<Goods> queryListByParams(GoodsSearchParams goodsSearchParams) {
        return this.list(goodsSearchParams.queryWrapper());
    }

    @Override
    @SystemLogPoint(description = "审核商品", customerLog = "'操作的商品ID:['+#goodsIds+']，操作后商品状态:['+#goodsAuthEnum+']'")
    @Transactional(rollbackFor = Exception.class)
    public boolean auditGoods(List<String> goodsIds, GoodsAuthEnum goodsAuthEnum) {
        return getGoodsMarketAuditManager().auditGoods(goodsIds, goodsAuthEnum);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    @SystemLogPoint(description = "商品状态操作", customerLog = "'操作类型:['+#goodsStatusEnum+']，操作对象:['+#goodsIds+']，操作原因:['+#underReason+']'")
    public Boolean updateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        return getGoodsMarketAuditManager().updateGoodsMarketAble(goodsIds, goodsStatusEnum, underReason);
    }

    @Override
    @SystemLogPoint(description = "店铺关闭下架商品", customerLog = "'操作类型:['+#goodsStatusEnum+']，操作对象:['+#storeId+']，操作原因:['+#underReason+']'")
    public Boolean updateGoodsMarketAbleByStoreId(String storeId, GoodsStatusEnum goodsStatusEnum, String underReason) {
        return getGoodsMarketAuditManager().updateGoodsMarketAbleByStoreId(storeId, goodsStatusEnum, underReason);
    }

    @Override
    @SystemLogPoint(description = "管理员关闭下架商品", customerLog = "'操作类型:['+#goodsStatusEnum+']，操作对象:['+#goodsIds+']，操作原因:['+#underReason+']'")
    @Transactional(rollbackFor = Exception.class)
    public Boolean managerUpdateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        return getGoodsMarketAuditManager().managerUpdateGoodsMarketAble(goodsIds, goodsStatusEnum, underReason);
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

        LambdaQueryWrapper<Goods> queryWrapper = this.getQueryWrapperByStoreAuthority();
        queryWrapper.in(Goods::getId, goodsIds);
        List<Goods> goodsList = this.list(queryWrapper);
        List<String> goodsCacheKeys = new ArrayList<>();
        for (Goods goods : goodsList) {
            this.goodsSkuService.updateGoodsSkuStatus(goods);
            goodsCacheKeys.add(CachePrefix.GOODS.getPrefix() + goods.getId());
            if (GoodsTypeEnum.E_COUPON.name().equals(goods.getGoodsType())) {
                List<GoodsSku> skus = goodsSkuService.getGoodsSkuListByGoodsId(goods.getId());
                for (GoodsSku sku : skus) {
                    cardKeyPoolSupport.softDeletePoolOnSkuRemove(sku.getId());
                }
            }
        }
        cache.multiDel(goodsCacheKeys);
        this.notifyGoodsDelete(goodsIds);
        return true;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean freight(List<String> goodsIds, String templateId) {
        if (goodsIds == null || goodsIds.isEmpty()) {
            return true;
        }

        AuthUser authUser = this.checkStoreAuthority();

        FreightTemplate freightTemplate = freightTemplateService.getById(templateId);
        if (freightTemplate == null) {
            throw new ServiceException(ResultCode.FREIGHT_TEMPLATE_NOT_EXIST);
        }
        if (authUser != null && !freightTemplate.getStoreId().equals(authUser.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }

        List<String> distinctGoodsIds = goodsIds.stream().distinct().collect(Collectors.toList());
        if (authUser != null) {
            long count = this.count(new LambdaQueryWrapper<Goods>()
                    .in(Goods::getId, distinctGoodsIds)
                    .eq(Goods::getStoreId, authUser.getStoreId()));
            if (count != distinctGoodsIds.size()) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }

        LambdaUpdateWrapper<Goods> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.set(Goods::getTemplateId, templateId);
        lambdaUpdateWrapper.in(Goods::getId, distinctGoodsIds);
        if (authUser != null) {
            lambdaUpdateWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        }
        List<String> goodsCache = distinctGoodsIds.stream().map(item -> CachePrefix.GOODS.getPrefix() + item).collect(Collectors.toList());
        cache.multiDel(goodsCache);
        goodsSkuService.freight(distinctGoodsIds, templateId);
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public void updateStock(String goodsId) {
        stockAggregates.updateStock(goodsId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsCommentNum(String goodsId, String skuId) {
        GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(skuId);
        if (goodsSku == null) {
            return;
        }

        Goods goods = this.getById(goodsId);
        EvaluationQueryParams queryParams = new EvaluationQueryParams();
        queryParams.setGoodsId(goodsId);
        long count = memberEvaluationMapper.selectCount(queryParams.queryWrapper());
        goods.setCommentNum((int) count);

        queryParams.setGrade("GOOD");
        long goodCount = memberEvaluationMapper.selectCount(queryParams.queryWrapper());
        if (count != 0) {
            double rate = (double) goodCount / count;
            rate = Math.round(rate * 100.0) / 100.0;
            goods.setGrade(rate * 100);
        } else {
            goods.setGrade(100.0);
        }

        this.updateById(goods);
        goodsSkuService.updateGoodsSkuGrade(goodsId, goods.getGrade(), goods.getCommentNum());
        cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
    }

    @Override
    public void updateGoodsBuyCount(String goodsId, int buyCount) {
        this.update(new LambdaUpdateWrapper<Goods>().eq(Goods::getId, goodsId).set(Goods::getBuyCount, buyCount));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStoreDetail(Store store) {
        this.update(new LambdaUpdateWrapper<Goods>()
                .eq(Goods::getStoreId, store.getId())
                .set(Goods::getStoreName, store.getStoreName())
                .set(Goods::getSelfOperated, store.getSelfOperated()));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void scheduleGoodsMarket(List<String> goodsIds, GoodsStatusEnum status, Date triggerTime, String reason) {
        getGoodsMarketAuditManager().scheduleGoodsMarket(goodsIds, status, triggerTime, reason);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cancelGoodsMarketSchedule(String goodsId, GoodsStatusEnum status) {
        getGoodsMarketAuditManager().cancelGoodsMarketSchedule(goodsId, status);
    }

    @Override
    public long countStoreGoodsNum(String storeId) {
        return this.count(new LambdaQueryWrapper<Goods>().eq(Goods::getStoreId, storeId));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void categoryGoodsName(String categoryId) {
        List<Goods> list = this.list(new LambdaQueryWrapper<Goods>().like(Goods::getCategoryPath, categoryId));
        list.parallelStream().forEach(goods -> {
            cache.remove(CachePrefix.GOODS.getPrefix() + goods.getId());
        });
    }

    @Override
    public void addGoodsCommentNum(Integer commentNum, String goodsId) {
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.set(Goods::getCommentNum, commentNum);
        updateWrapper.eq(Goods::getId, goodsId);
        this.update(updateWrapper);
    }

    @Transactional
    public void notifyGoodsDelete(List<String> goodsIds) {
        getGoodsMarketAuditManager().notifyGoodsDelete(goodsIds);
    }

    private void setGoodsGalleryParam(String origin, Goods goods) {
        GoodsGallery goodsGallery = goodsGalleryService.getGoodsGallery(origin);
        goods.setOriginal(goodsGallery.getOriginal());
        goods.setSmall(goodsGallery.getSmall());
        goods.setThumbnail(goodsGallery.getThumbnail());
    }

    private void checkGoods(Goods goods) {
        switch (goods.getGoodsType()) {
            case "PHYSICAL_GOODS":
                if ("0".equals(goods.getTemplateId())) {
                    throw new ServiceException(ResultCode.PHYSICAL_GOODS_NEED_TEMP);
                }
                break;
            case "VIRTUAL_GOODS":
            case "E_COUPON":
            case "APPOINTMENT_GOODS":
                if (!"0".equals(goods.getTemplateId())) {
                    goods.setTemplateId("0");
                }
                break;
            default:
                throw new ServiceException(ResultCode.GOODS_TYPE_ERROR);
        }

        Goods oldGoods = null;
        if (goods.getId() != null) {
            oldGoods = this.checkExist(goods.getId());
        } else {
            goods.setCommentNum(0);
            goods.setBuyCount(0);
            goods.setQuantity(0);
            goods.setGrade(100.0);
        }

        Setting setting = settingService.get(SettingEnum.GOODS_SETTING.name());
        GoodsSetting goodsSetting = JSON.parseObject(setting.getSettingValue(), GoodsSetting.class);
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        if (currentUser.getRole().equals(UserEnums.STORE)) {
            if (oldGoods != null && !currentUser.getStoreId().equals(oldGoods.getStoreId())) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
            StoreVO storeDetail = this.storeService.getStoreDetail();
            goods.setStoreId(storeDetail.getId());
            goods.setStoreName(storeDetail.getStoreName());
            goods.setSelfOperated(storeDetail.getSelfOperated());
            Boolean needsReview = goodsSetting.getGoodsCheck();
            if (Boolean.TRUE.equals(storeDetail.getSelfOperated())
                    && goodsSetting.getSelfOperatedGoodsCheck() != null) {
                needsReview = goodsSetting.getSelfOperatedGoodsCheck();
            }
            goods.setAuthFlag(Boolean.TRUE.equals(needsReview)
                    ? GoodsAuthEnum.TOBEAUDITED.name() : GoodsAuthEnum.PASS.name());
        } else {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
    }

    private Goods checkExist(String goodsId) {
        Goods goods = getById(goodsId);
        if (goods == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return goods;
    }

    private LambdaUpdateWrapper<Goods> getUpdateWrapperByStoreAuthority() {
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        AuthUser authUser = this.checkStoreAuthority();
        updateWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        return updateWrapper;
    }

    private AuthUser checkStoreAuthority() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null || currentUser.getRole() == null || !currentUser.getRole().equals(UserEnums.STORE)) {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        return currentUser;
    }

    private AuthUser checkManagerAuthority() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null || currentUser.getRole() == null || !currentUser.getRole().equals(UserEnums.MANAGER)) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return currentUser;
    }

    private LambdaQueryWrapper<Goods> getQueryWrapperByStoreAuthority() {
        LambdaQueryWrapper<Goods> queryWrapper = new LambdaQueryWrapper<>();
        AuthUser authUser = this.checkStoreAuthority();
        queryWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        return queryWrapper;
    }
}
