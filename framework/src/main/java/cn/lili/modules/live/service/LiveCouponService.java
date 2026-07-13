package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.dos.LiveCoupon;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveCouponService extends IService<LiveCoupon> {
    /**
     * 批量保存直播优惠券
     *
     * @param liveCouponList 直播优惠券列表
     */
    void saveBatchLiveCoupon(List<LiveCoupon> liveCouponList);

     /**
      * 根据直播间ID查询直播优惠券列表
      *
      * @param liveRoomId 直播间ID
      * @return 直播优惠券列表
      */
    List<LiveCoupon> liveCouponList(String liveRoomId);

     /**
      * 根据直播间ID删除直播优惠券
      *
      * @param liveRoomId 直播间ID
      */
    void deleteByLiveRoomId(String liveRoomId);

    /**
     * 根据直播间ID 从缓存获取直播商品列表
     *
     * @param liveId 直播间ID
     * @return 直播优惠券列表
     */
    List<LiveCoupon> getByCache(String liveId);

    /**
     * 设置推荐优惠券（同一时间只能有一个推荐优惠券）
     * @param id 优惠券ID
     * @return 是否成功
     */
    boolean setRecommend(String id);

    /**
     * 取消推荐优惠券
     * @param id 优惠券ID
     * @return 是否成功
     */
    boolean cancelRecommend(String id);

    /**
     * 删除直播优惠券
     * @param ids 直播优惠券Ids
     * @return
     */
    boolean removeLiveCoupon(List<String> ids);

    /**
     * 根据直播间ID和优惠券ID查询直播优惠券
     *
     * @param liveRoomId 直播间ID
     * @param couponId   优惠券ID
     * @return 直播优惠券
     */
    LiveCoupon getByLiveRoomIdAndCouponId(String liveRoomId, String couponId);
}
