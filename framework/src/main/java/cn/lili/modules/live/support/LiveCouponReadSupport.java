package cn.lili.modules.live.support;

import cn.lili.modules.live.entity.dos.LiveCoupon;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.mapper.LiveCouponMapper;
import cn.lili.modules.live.mapper.LiveRoomMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.stereotype.Component;

/**
 * 直播领券基础查询，读取直播间及关联券，避免会员领券依赖包含订单、推流和消息发送的直播服务。
 * 仅提供当前存储数据，直播状态、领取资格和异常提示仍由业务调用方校验。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class LiveCouponReadSupport {

    private final LiveRoomMapper roomMapper;
    private final LiveCouponMapper couponMapper;

    /**
     * 注入直播间和直播券存储，不调用完整直播业务服务。
     *
     * @param roomMapper 直播间存储
     * @param couponMapper 直播券关联存储
     */
    public LiveCouponReadSupport(LiveRoomMapper roomMapper, LiveCouponMapper couponMapper) {
        this.roomMapper = roomMapper;
        this.couponMapper = couponMapper;
    }

    /**
     * 按主键读取直播间，沿用存储层逻辑删除规则，不额外限制直播状态。
     *
     * @param liveRoomId 直播间主键
     * @return 直播间，不存在时为null
     */
    public LiveRoom getLiveRoomById(String liveRoomId) {
        return roomMapper.selectById(liveRoomId);
    }

    /**
     * 按直播间和优惠券主键读取关联，保留查到多条记录时抛出异常的原有行为。
     * 不读取直播券列表缓存，不额外限定推荐状态或领取资格。
     *
     * @param liveRoomId 直播间主键
     * @param couponId 优惠券主键
     * @return 直播券关联，不存在时为null
     */
    public LiveCoupon getByLiveRoomIdAndCouponId(String liveRoomId, String couponId) {
        return couponMapper.selectOne(new LambdaQueryWrapper<LiveCoupon>()
                .eq(LiveCoupon::getLiveRoomId, liveRoomId)
                .eq(LiveCoupon::getCouponId, couponId));
    }
}
