package cn.lili.modules.live.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.live.entity.dos.LiveCoupon;
import cn.lili.modules.live.mapper.LiveCouponMapper;
import cn.lili.modules.live.service.LiveCouponService;
import cn.lili.modules.live.support.LiveCouponReadSupport;
import cn.lili.modules.live.util.MqttUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
@RequiredArgsConstructor
public class LiveCouponServiceImpl extends ServiceImpl<LiveCouponMapper, LiveCoupon> implements LiveCouponService {

    @Autowired
    private Cache cache;
    @Autowired
    private MqttUtil mqttUtil;
    @Autowired
    private LiveCouponReadSupport couponReads;

    private final String COUPON = "coupon/";

    @Override
    public void saveBatchLiveCoupon(List<LiveCoupon> liveCouponList) {
        //先清空，再保存
        deleteByLiveRoomId(liveCouponList.get(0).getLiveRoomId());
        this.saveBatch(liveCouponList);
        sendLiveCouponMessage(liveCouponList.get(0).getLiveRoomId(), liveCouponList);

    }

    @Override
    public List<LiveCoupon> liveCouponList(String liveRoomId) {
        return this.list(new LambdaQueryWrapper<LiveCoupon>().eq(LiveCoupon::getLiveRoomId, liveRoomId));
    }

    @Override
    public void deleteByLiveRoomId(String liveRoomId) {
        this.remove(new LambdaQueryWrapper<LiveCoupon>().eq(LiveCoupon::getLiveRoomId, liveRoomId));
    }

    @Override
    public List<LiveCoupon> getByCache(String liveId) {
        Object object = cache.get(CachePrefix.LIVE_COUPON_LIST.getPrefix() + liveId);
        if(object != null){
            return (List<LiveCoupon>) object;
        }else{
            List<LiveCoupon> liveCouponList = liveCouponList(liveId);
            setCache(liveId, liveCouponList);
            return liveCouponList;
        }
    }

    public void setCache(String liveId, List<LiveCoupon> liveCouponList) {
        cache.put(CachePrefix.LIVE_COUPON_LIST.getPrefix() + liveId, liveCouponList);
    }

    @Override
    public boolean setRecommend(String id) {
        LiveCoupon liveCoupon = this.getById(id);
        // 取消所有商品的推荐状态
        LambdaUpdateWrapper<LiveCoupon> clearRecommend = new LambdaUpdateWrapper<>();
        clearRecommend.eq(LiveCoupon::getLiveRoomId, liveCoupon.getLiveRoomId()).eq(LiveCoupon::getRecommend, true);
        clearRecommend.set(LiveCoupon::getRecommend, false);
        this.update(clearRecommend);
        // 设置当前商品为推荐
        liveCoupon.setRecommend(true);
        return updateAndSendMqtt(liveCoupon);
    }

    @Override
    public boolean cancelRecommend(String id) {
        LiveCoupon liveCoupon = this.getById(id);
        liveCoupon.setRecommend(false);
        return updateAndSendMqtt(liveCoupon);
    }

    @Override
    public boolean removeLiveCoupon(List<String> ids) {
        return this.removeByIds(ids);
    }

    /**
     * 复用直播领券查询，按直播间及优惠券主键读取唯一关联，多条记录时仍抛出异常。
     *
     * @param liveRoomId 直播间主键
     * @param couponId 优惠券主键
     * @return 直播券关联，不存在时为null
     */
    @Override
    public LiveCoupon getByLiveRoomIdAndCouponId(String liveRoomId, String couponId) {
        return couponReads.getByLiveRoomIdAndCouponId(liveRoomId, couponId);
    }

    private boolean updateAndSendMqtt(LiveCoupon liveCoupon) {
        boolean update = this.updateById(liveCoupon);
        sendLiveCouponMessage(liveCoupon.getLiveRoomId(), null);
        return update;
    }

    void sendLiveCouponMessage(String liveRoomId, List<LiveCoupon> liveCouponList){
        if(liveCouponList == null){
            liveCouponList = liveCouponList(liveRoomId);
        }

        cache.put(CachePrefix.LIVE_COUPON_LIST.getPrefix() + liveRoomId, liveCouponList, 86400L);

        String jsonStr = JSONUtil.toJsonStr(liveCouponList);
        mqttUtil.sendSensorData(COUPON + liveRoomId, jsonStr);
    }
}
