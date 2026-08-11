package cn.lili.modules.live.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.live.entity.dos.LiveGoods;
import cn.lili.modules.live.mapper.LiveGoodsMapper;
import cn.lili.modules.live.service.LiveGoodsService;
import cn.lili.modules.live.util.MqttUtil;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.service.OrderItemService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.stream.Collectors;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
@RequiredArgsConstructor
public class LiveGoodsServiceImpl extends ServiceImpl<LiveGoodsMapper, LiveGoods> implements LiveGoodsService {

    @Autowired
    private Cache cache;

    @Autowired
    private OrderItemService orderItemService;

    @Autowired
    private MqttUtil mqttUtil;

    private final String GOODS = "goods/";

    @Override
    public void saveBatchLiveGoodsArray(List<LiveGoods> liveGoodsArray) {

        //先获取数据库数据，如果数据有重复的，则略过
        String liveId = liveGoodsArray.get(0).getLiveRoomId();
        this.queryByLiveId(liveId).forEach(item -> {
            liveGoodsArray.removeIf(liveGoods -> item.getSkuId().equals(liveGoods.getSkuId()));
        });

        if (liveGoodsArray.isEmpty()) {
            return; // 如果没有新的直播商品，则不进行保存
        }
        Random random = new Random();
        for (LiveGoods liveGoods : liveGoodsArray) {
            liveGoods.setPopularity(random.nextInt(800 - 100 + 1) + 100);
        }


        // 批量保存新的直播商品
        this.saveBatch(liveGoodsArray);

        this.sendLiveGoodsData(liveId, null);
    }

    @Override
    public List<LiveGoods> queryByLiveId(String liveRoomId) {
        if (liveRoomId == null) {
            return Collections.emptyList();
        }
        LambdaQueryWrapper<LiveGoods> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(LiveGoods::getLiveRoomId, liveRoomId);
        queryWrapper.orderByDesc(LiveGoods::getCreateTime);
        return this.list(queryWrapper);
    }

    @Override
    public List<LiveGoods> getByCache(String liveRoomId) {
        if (liveRoomId == null) {
            return Collections.emptyList();
        }
        List<LiveGoods> liveGoodsList = new ArrayList<>();
        //查看缓存中是否存在列表，如果存在从缓存中获取
        String cacheKey = getCacheKey(liveRoomId);
        liveGoodsList = (List<LiveGoods>) cache.get(cacheKey);
        if (liveGoodsList == null) {
            //缓存中不存在，从数据库获取
            LambdaQueryWrapper<LiveGoods> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.eq(LiveGoods::getLiveRoomId, liveRoomId);
            liveGoodsList = this.list(queryWrapper);
            //将数据缓存到redis中，设置过期时间为10分钟
            cache.put(cacheKey, liveGoodsList, 10 * 60L);
        }
        return liveGoodsList;
    }

    @Override
    public boolean batchUpdateShowStatus(List<String> ids, boolean show) {
        if (ids == null || ids.isEmpty()) {
            return false;
        }

        LambdaUpdateWrapper<LiveGoods> wrapper = new LambdaUpdateWrapper<>();
        wrapper.in(LiveGoods::getId, ids).set(LiveGoods::getHideFlag, show);
        boolean update = this.update(wrapper);
        //发送mqtt
        LiveGoods liveGoods = checkLiveGoods(ids.get(0));
        this.sendLiveGoodsData(liveGoods.getLiveRoomId(), null);

        return update;
    }

    @Override
    public boolean setRecommend(String id) {
        LiveGoods goods = checkLiveGoods(id);
        // 取消所有商品的推荐状态
        LambdaUpdateWrapper<LiveGoods> clearRecommend = new LambdaUpdateWrapper<>();
        clearRecommend.eq(LiveGoods::getLiveRoomId, goods.getLiveRoomId()).eq(LiveGoods::getRecommend, true);
        clearRecommend.set(LiveGoods::getRecommend, false);
        this.update(clearRecommend);
        this.removeCache(goods.getLiveRoomId());
        // 设置当前商品为推荐
        goods.setRecommend(true);
        return updateAndSendMqtt(goods);
    }

    @Override
    public boolean cancelRecommend(String id) {
        LiveGoods goods = checkLiveGoods(id);
        goods.setRecommend(false);
        //修改
        return updateAndSendMqtt(goods);
    }

    @Override
    public List<LiveGoods> getLiveGoodsByIdsAndAddNum(Order order) {

        List<OrderItem> orderItemList = orderItemService.getByOrderSn(order.getSn());
        List<String> liveGoodsIdList =
                orderItemList.stream().map(OrderItem::getSkuId).collect(Collectors.toList());
        List<LiveGoods> liveGoods = new ArrayList<>();
        //如果是秒杀，通过活动商品Id来获取

        liveGoods = this.getLiveGoodsById(liveGoodsIdList);


        if (!CollUtil.isEmpty(liveGoods)) {
            // 增加购买数量
            liveGoods.forEach(item -> {
                for (OrderItem orderItem : orderItemList) {
                    if (item.getSkuId().equals(orderItem.getSkuId())) {
                        LambdaUpdateWrapper<LiveGoods> updateWrapper = new LambdaUpdateWrapper<>();

                        updateWrapper.eq(LiveGoods::getSkuId, orderItem.getSkuId()) // 条件
                                .setSql("order_num = IFNULL(order_num, 0) + 1")
                                .setSql("buy_count = IFNULL(buy_count, 0)+" + orderItem.getNum());
                        this.update(updateWrapper);
                    }
                }
            });
            //发送mqtt
            this.sendLiveGoodsData(liveGoods.get(0).getLiveRoomId(), null);
            return liveGoods;
        }
        return liveGoods;
    }


    @Override
    public boolean removeLiveGoods(List<String> ids) {
        //发送mqtt
        LiveGoods liveGoods = checkLiveGoods(ids.get(0));
        boolean remove = this.removeByIds(ids);
        sendLiveGoodsData(liveGoods.getLiveRoomId(), null);
        return remove;
    }

    @Override
    public boolean setSoldOut(String id, boolean soldOutFlag) {
        LiveGoods goods = checkLiveGoods(id);
        goods.setSoldOutFlag(soldOutFlag);

        return updateAndSendMqtt(goods);
    }

    @Override
    public void setPopularity(String id, String liveId, Integer popularity) {
        List<LiveGoods> liveGoodsList = getByCache(liveId);
        for (LiveGoods liveGoods : liveGoodsList) {
            if (liveGoods.getId().equals(id)) {
                liveGoods.setPopularity(popularity);
                String key = CachePrefix.LIVE_GOODS_POPULARITY.getPrefix() + id + "_" + liveId;
                cache.put(key, popularity);
            }
        }

        //预防存入一个缓存，防止mqtt断线轮询请求数据库
        cache.put(getCacheKey(liveId), liveGoodsList, 86400L);

        sendLiveGoodsData(liveId, liveGoodsList);
    }


    @Override
    public void addPopularity(String id, String liveId) {
        List<LiveGoods> liveGoodsList = getByCache(liveId);
        Random random = new Random();
        for (LiveGoods liveGoods : liveGoodsList) {
            if (liveGoods.getId().equals(id)) {
                int randomNum = random.nextInt(5 - 3 + 1) + 3;
                String key = CachePrefix.LIVE_GOODS_POPULARITY.getPrefix() + id + "_" + liveId;
                Long popularity = cache.incr(key, randomNum);
                liveGoods.setPopularity(popularity.intValue());
            }
        }

        //预防存入一个缓存，防止mqtt断线轮询请求数据库
        cache.put(getCacheKey(liveId), liveGoodsList, 86400L);

        String tsKey = "live:popularity:ts:" + liveId;
        Long lastTs = (Long) cache.get(tsKey);
        long now = System.currentTimeMillis();

        // 每 10 秒固定发送一次
        if (lastTs == null || now - lastTs >= 5_000) {
            sendLiveGoodsData(liveId, liveGoodsList);
            cache.put(tsKey, now, 86400L);
        }
    }

    @Override
    public Long getPopularity(String id, String liveId) {
        String key = CachePrefix.LIVE_GOODS_POPULARITY.getPrefix() + id + "_" + liveId;
        Object object = cache.get(key);
        if (object != null) {
            Long popularity = Long.parseLong(cache.get(key).toString());
            return popularity == null ? 0L : popularity;
        }
        return 0L;
    }

    @Override
    public List<LiveGoods> getLiveGoodsById(List<String> ids) {
        if (ids == null) {
            // 如果ID为null，直接返回null
            return null;
        }
        return this.list(new LambdaQueryWrapper<LiveGoods>().in(LiveGoods::getSkuId, ids));
    }

    private String getCacheKey(String liveId) {
        return CachePrefix.LIVE_GOODS_LIST.getPrefix() + liveId;
    }

    private LiveGoods checkLiveGoods(String id) {
        LiveGoods goods = this.getById(id);
        if (goods == null) {
            throw new ServiceException(ResultCode.LIVE_GOODS_NOT_EXIST);
        }
        return goods;
    }

    private boolean updateAndSendMqtt(LiveGoods liveGoods) {
        boolean update = this.updateById(liveGoods);
        //发送mqtt消息
        this.sendLiveGoodsData(liveGoods.getLiveRoomId(), null);
        //返回修改状态
        return update;
    }

    private void sendLiveGoodsData(String liveRoomId, List<LiveGoods> liveGoodsList) {
        //获取全部直播商品
        if (liveGoodsList == null) {
            liveGoodsList = this.queryByLiveId(liveRoomId);
        }
        for (LiveGoods liveGoods : liveGoodsList) {
            if (liveGoods.getRecommend()) {
                liveGoods.setPopularity(getPopularity(liveGoods.getId(), liveRoomId).intValue());
            }
        }

        //预防存入一个缓存，防止mqtt断线轮询请求数据库
        cache.put(getCacheKey(liveRoomId), liveGoodsList, 86400L);

        // 发送mqtt
        String jsonStr = JSONUtil.toJsonStr(liveGoodsList);
        mqttUtil.sendSensorData(GOODS + liveRoomId, jsonStr);
    }

    private void removeCache(String liveRoomId){
        cache.remove(getCacheKey(liveRoomId));
    }
}
