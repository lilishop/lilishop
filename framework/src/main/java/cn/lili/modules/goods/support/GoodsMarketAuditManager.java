package cn.lili.modules.goods.support;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import cn.lili.trigger.enums.DelayTypeEnums;
import cn.lili.trigger.interfaces.TimeTrigger;
import cn.lili.trigger.message.GoodsMarketMessage;
import cn.lili.trigger.model.TimeExecuteConstant;
import cn.lili.trigger.model.TimeTriggerMsg;
import cn.lili.trigger.util.DelayQueueTools;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 商品审核、上下架及定时上下架管理器，SPU读写统一使用Mapper，保留SKU联动、缓存和事件处理
 * <p>
 * 集中管理 SPU 的审核流转、平台/商家上下架控制、定时延时队列任务调度以及商品状态变更事件广播。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
@NoArgsConstructor
public class GoodsMarketAuditManager {

    @Autowired
    private GoodsMapper goodsMapper;

    @Autowired
    private GoodsStoreMarketSupport storeMarkets;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Autowired
    private Cache<GoodsVO> cache;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private TimeTrigger timeTrigger;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    /**
     * 全参构造函数，用于单元测试与非 Spring 环境装配
     *
     * @param goodsMapper              商品映射器
     * @param storeMarkets             按店铺上下架组件
     * @param goodsSkuService          SKU 业务服务
     * @param cache                    商品 VO 缓存
     * @param liliMessageTemplate      消息发送模板
     * @param rocketmqCustomProperties RocketMQ 配置
     * @param timeTrigger              延时队列调度器
     * @param applicationEventPublisher 领域事件发布器
     */
    public GoodsMarketAuditManager(GoodsMapper goodsMapper,
                                   GoodsStoreMarketSupport storeMarkets,
                                   GoodsSkuService goodsSkuService,
                                   Cache<GoodsVO> cache,
                                   LiliMessageTemplate liliMessageTemplate,
                                   RocketmqCustomProperties rocketmqCustomProperties,
                                   TimeTrigger timeTrigger,
                                   ApplicationEventPublisher applicationEventPublisher) {
        this.goodsMapper = goodsMapper;
        this.storeMarkets = storeMarkets;
        this.goodsSkuService = goodsSkuService;
        this.cache = cache;
        this.liliMessageTemplate = liliMessageTemplate;
        this.rocketmqCustomProperties = rocketmqCustomProperties;
        this.timeTrigger = timeTrigger;
        this.applicationEventPublisher = applicationEventPublisher;
    }

    /**
     * 批量审核商品并联动更新 SKU 状态与通知 MQ
     *
     * @param goodsIds      待审核的商品 ID 列表
     * @param goodsAuthEnum 目标审核状态（PASS/REFUSE）
     * @return 审核操作是否成功
     */
    @Transactional(rollbackFor = Exception.class)
    public boolean auditGoods(List<String> goodsIds, GoodsAuthEnum goodsAuthEnum) {
        List<String> goodsCacheKeys = new ArrayList<>();
        boolean result = false;
        for (String goodsId : goodsIds) {
            Goods goods = this.checkExist(goodsId);
            goods.setAuthFlag(goodsAuthEnum.name());
            result = this.goodsMapper.updateById(goods) > 0;
            if (this.goodsSkuService != null) {
                this.goodsSkuService.updateGoodsSkuStatus(goods);
            }
            goodsCacheKeys.add(CachePrefix.GOODS.getPrefix() + goodsId);
            if (rocketmqCustomProperties != null && liliMessageTemplate != null) {
                String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_AUDIT.name();
                liliMessageTemplate.asyncSend(destination, JSON.toJSONString(goods), MessageSendCallbackBuilder.commonCallback());
            }
        }
        if (cache != null && !goodsCacheKeys.isEmpty()) {
            cache.multiDel(goodsCacheKeys);
        }
        return result;
    }

    /**
     * 店铺端批量更新商品上下架状态
     *
     * @param goodsIds        商品 ID 集合
     * @param goodsStatusEnum 目标上下架状态
     * @param underReason     下架原因（仅下架时传递）
     * @return 更新结果
     */
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        if (goodsIds == null || goodsIds.isEmpty()) {
            return true;
        }

        LambdaUpdateWrapper<Goods> updateWrapper = this.getUpdateWrapperByStoreAuthority();
        updateWrapper.set(Goods::getMarketEnable, goodsStatusEnum.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        if (GoodsStatusEnum.UPPER.equals(goodsStatusEnum)) {
            updateWrapper.set(Goods::getScheduledUpperTime, null);
            updateWrapper.set(Goods::getScheduledUpperReason, null);
        } else if (GoodsStatusEnum.DOWN.equals(goodsStatusEnum)) {
            updateWrapper.set(Goods::getScheduledDownTime, null);
            updateWrapper.set(Goods::getScheduledDownReason, null);
        }
        updateWrapper.in(Goods::getId, goodsIds);
        boolean result = this.goodsMapper.update(null, updateWrapper) > 0;

        this.updateGoodsStatus(goodsIds, goodsStatusEnum);
        return result;
    }

    /**
     * 按店铺维度关闭或下架全部商品
     *
     * @param storeId         店铺 ID
     * @param goodsStatusEnum 目标上下架状态
     * @param underReason     下架原因
     * @return 更新结果
     */
    public Boolean updateGoodsMarketAbleByStoreId(String storeId, GoodsStatusEnum goodsStatusEnum, String underReason) {
        return storeMarkets.updateByStoreId(storeId, goodsStatusEnum, underReason);
    }

    /**
     * 管理端强制批量下架或上架商品
     *
     * @param goodsIds        商品 ID 集合
     * @param goodsStatusEnum 目标状态
     * @param underReason     下架原因
     * @return 操作结果
     */
    @Transactional(rollbackFor = Exception.class)
    public Boolean managerUpdateGoodsMarketAble(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum, String underReason) {
        if (goodsIds == null || goodsIds.isEmpty()) {
            return true;
        }

        this.checkManagerAuthority();

        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.set(Goods::getMarketEnable, goodsStatusEnum.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        if (GoodsStatusEnum.UPPER.equals(goodsStatusEnum)) {
            updateWrapper.set(Goods::getScheduledUpperTime, null);
            updateWrapper.set(Goods::getScheduledUpperReason, null);
        } else if (GoodsStatusEnum.DOWN.equals(goodsStatusEnum)) {
            updateWrapper.set(Goods::getScheduledDownTime, null);
            updateWrapper.set(Goods::getScheduledDownReason, null);
        }
        updateWrapper.in(Goods::getId, goodsIds);
        boolean result = this.goodsMapper.update(null, updateWrapper) > 0;

        this.updateGoodsStatus(goodsIds, goodsStatusEnum);
        return result;
    }

    /**
     * 设置定时上下架延时任务
     *
     * @param goodsIds    商品 ID 列表
     * @param status      目标上下架状态
     * @param triggerTime 触发时间（秒级）
     * @param reason      上下架说明
     */
    @Transactional(rollbackFor = Exception.class)
    public void scheduleGoodsMarket(List<String> goodsIds, GoodsStatusEnum status, Date triggerTime, String reason) {
        if (goodsIds == null || goodsIds.isEmpty() || triggerTime == null) {
            return;
        }
        if (triggerTime.getTime() <= System.currentTimeMillis()) {
            throw new ServiceException("定时时间必须晚于当前时间");
        }
        AuthUser user = UserContext.getCurrentUser();
        boolean manager = user != null && user.getRole() == UserEnums.MANAGER;
        if (!manager && checkStoreAuthority() == null) {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        List<String> distinctGoodsIds = goodsIds.stream().distinct().collect(Collectors.toList());
        List<Goods> oldGoodsList = this.goodsMapper.selectBatchIds(distinctGoodsIds);
        if (oldGoodsList.size() != distinctGoodsIds.size()) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        if (!manager) {
            boolean allMatch = oldGoodsList.stream().allMatch(g -> user.getStoreId().equals(g.getStoreId()));
            if (!allMatch) {
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
            }
        }
        String executor = GoodsStatusEnum.UPPER.equals(status)
                ? TimeExecuteConstant.GOODS_UPPER_EXECUTOR
                : TimeExecuteConstant.GOODS_DOWN_EXECUTOR;

        LambdaUpdateWrapper<Goods> uw = new LambdaUpdateWrapper<>();
        uw.in(Goods::getId, distinctGoodsIds);
        if (!manager) {
            uw.eq(Goods::getStoreId, user.getStoreId());
        }
        long normalizedMs = (triggerTime.getTime() / 1000L) * 1000L;
        Date scheduledDate = new Date(normalizedMs);
        if (GoodsStatusEnum.UPPER.equals(status)) {
            uw.set(Goods::getScheduledUpperTime, scheduledDate);
            uw.set(Goods::getScheduledUpperReason, reason);
        } else {
            uw.set(Goods::getScheduledDownTime, scheduledDate);
            uw.set(Goods::getScheduledDownReason, reason);
        }
        this.goodsMapper.update(null, uw);
        for (String goodsId : distinctGoodsIds) {
            String uniqueKey = DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.GOODS, goodsId);
            Date oldTime = oldGoodsList.stream()
                    .filter(g -> g.getId().equals(goodsId))
                    .map(g -> GoodsStatusEnum.UPPER.equals(status) ? g.getScheduledUpperTime() : g.getScheduledDownTime())
                    .filter(Objects::nonNull)
                    .findFirst()
                    .orElse(null);
            if (oldTime != null && timeTrigger != null && rocketmqCustomProperties != null) {
                timeTrigger.delete(executor, (oldTime.getTime() / 1000L) * 1000L, uniqueKey,
                        rocketmqCustomProperties.getPromotionTopic());
            }
            if (timeTrigger != null && rocketmqCustomProperties != null) {
                GoodsMarketMessage payload = new GoodsMarketMessage(Collections.singletonList(goodsId), reason, normalizedMs);
                TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(executor, normalizedMs, payload, uniqueKey,
                        rocketmqCustomProperties.getPromotionTopic());
                timeTrigger.addDelay(timeTriggerMsg);
            }
        }
    }

    /**
     * 取消商品定时上下架计划
     *
     * @param goodsId 商品 ID
     * @param status  定时类型（UPPER/DOWN）
     */
    @Transactional(rollbackFor = Exception.class)
    public void cancelGoodsMarketSchedule(String goodsId, GoodsStatusEnum status) {
        AuthUser user = UserContext.getCurrentUser();
        boolean manager = user != null && user.getRole() == UserEnums.MANAGER;
        if (!manager && checkStoreAuthority() == null) {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        Goods goods = checkExist(goodsId);
        if (!manager && !user.getStoreId().equals(goods.getStoreId())) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        if (status != GoodsStatusEnum.UPPER && status != GoodsStatusEnum.DOWN) {
            throw new ServiceException("不支持的定时计划类型");
        }
        boolean upper = status == GoodsStatusEnum.UPPER;
        Date time = upper ? goods.getScheduledUpperTime() : goods.getScheduledDownTime();
        if (time == null) {
            return;
        }

        LambdaUpdateWrapper<Goods> update = new LambdaUpdateWrapper<Goods>()
                .eq(Goods::getId, goodsId).eq(!manager, Goods::getStoreId, user.getStoreId());
        if (upper) {
            update.eq(Goods::getScheduledUpperTime, time)
                    .set(Goods::getScheduledUpperTime, null).set(Goods::getScheduledUpperReason, null);
        } else {
            update.eq(Goods::getScheduledDownTime, time)
                    .set(Goods::getScheduledDownTime, null).set(Goods::getScheduledDownReason, null);
        }
        boolean updateSuccess = this.goodsMapper.update(null, update) > 0;
        if (!updateSuccess) {
            throw new ServiceException("定时计划已变化，请刷新后重试");
        }
        if (cache != null) {
            cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
        }
        if (timeTrigger != null && rocketmqCustomProperties != null) {
            timeTrigger.delete(upper ? TimeExecuteConstant.GOODS_UPPER_EXECUTOR : TimeExecuteConstant.GOODS_DOWN_EXECUTOR,
                    time.getTime(), DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.GOODS, goodsId),
                    rocketmqCustomProperties.getPromotionTopic());
        }
    }

    /**
     * 更新商品状态及同步 SKU、广播状态变更事件
     *
     * @param goodsIds        商品 ID 集合
     * @param goodsStatusEnum 状态枚举
     */
    public void updateGoodsStatus(List<String> goodsIds, GoodsStatusEnum goodsStatusEnum) {
        if (this.goodsSkuService != null) {
            this.goodsSkuService.updateGoodsSkuStatusByGoodsIds(goodsIds, goodsStatusEnum.name());
        }

        if (cache != null && goodsIds != null && !goodsIds.isEmpty()) {
            List<String> goodsCacheKeys = goodsIds.stream()
                    .map(id -> CachePrefix.GOODS.getPrefix() + id)
                    .toList();
            cache.multiDel(goodsCacheKeys);
        }

        if (GoodsStatusEnum.DOWN.equals(goodsStatusEnum)) {
            this.notifyGoodsDelete(goodsIds);
        }

        if (goodsStatusEnum.equals(GoodsStatusEnum.DOWN) && applicationEventPublisher != null && rocketmqCustomProperties != null) {
            applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("下架商品",
                    rocketmqCustomProperties.getGoodsTopic(), GoodsTagsEnum.DOWN.name(), JSON.toJSONString(goodsIds)));
        }
    }

    /**
     * 发布商品删除消息
     *
     * @param goodsIds 已删除的商品 ID 列表
     */
    @Transactional
    public void notifyGoodsDelete(List<String> goodsIds) {
        if (goodsIds != null && !goodsIds.isEmpty() && applicationEventPublisher != null && rocketmqCustomProperties != null) {
            applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("删除商品消息", rocketmqCustomProperties.getGoodsTopic(),
                    GoodsTagsEnum.GOODS_DELETE.name(), JSON.toJSONString(goodsIds)));
        }
    }

    /**
     * 校验当前登录用户为店铺身份
     *
     * @return 登录店铺用户信息
     */
    public AuthUser checkStoreAuthority() {
        return storeMarkets.checkStoreAuthority();
    }

    /**
     * 校验当前登录用户为管理员身份
     *
     * @return 登录管理员用户信息
     */
    public AuthUser checkManagerAuthority() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null || currentUser.getRole() == null || !currentUser.getRole().equals(UserEnums.MANAGER)) {
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
        return currentUser;
    }

    /**
     * 构造带当前店铺权限限定条件的更新包装器
     *
     * @return 绑定 storeId 的 LambdaUpdateWrapper
     */
    public LambdaUpdateWrapper<Goods> getUpdateWrapperByStoreAuthority() {
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        AuthUser authUser = this.checkStoreAuthority();
        updateWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        return updateWrapper;
    }

    /**
     * 构造带当前店铺权限限定条件的查询包装器
     *
     * @return 绑定 storeId 的 LambdaQueryWrapper
     */
    public LambdaQueryWrapper<Goods> getQueryWrapperByStoreAuthority() {
        LambdaQueryWrapper<Goods> queryWrapper = new LambdaQueryWrapper<>();
        AuthUser authUser = this.checkStoreAuthority();
        queryWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        return queryWrapper;
    }

    /**
     * 检查商品是否存在
     *
     * @param goodsId 商品 ID
     * @return 商品实体
     */
    private Goods checkExist(String goodsId) {
        Goods goods = this.goodsMapper.selectById(goodsId);
        if (goods == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        return goods;
    }
}
