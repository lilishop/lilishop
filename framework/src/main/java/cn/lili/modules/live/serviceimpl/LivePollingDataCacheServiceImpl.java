package cn.lili.modules.live.serviceimpl;

import cn.lili.modules.live.entity.dos.LiveCoupon;
import cn.lili.modules.live.entity.dos.LiveGoods;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.entity.vos.LivePollingDataVO;
import cn.lili.modules.live.service.LiveCouponService;
import cn.lili.modules.live.service.LiveGoodsService;
import cn.lili.modules.live.service.LivePollingDataCacheService;
import cn.lili.modules.live.service.LiveRoomService;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
@RequiredArgsConstructor
public class LivePollingDataCacheServiceImpl implements LivePollingDataCacheService {

    private final LiveRoomService liveRoomService;

    private final LiveGoodsService liveGoodsService;

    private final LiveCouponService liveCouponService;

    @Override
    public LivePollingDataVO getGlobalPollingData(String liveId) {

        LiveRoom liveDetail = liveRoomService.getLiveRoomUserDetail(liveId);
        List<LiveGoods> goodsList = liveGoodsService.getByCache(liveId);
        List<LiveCoupon> liveCoupon = liveCouponService.getByCache(liveId);

        return  LivePollingDataVO.builder()
                .liveDetail(liveDetail)
                .goodsList(goodsList)
                .liveCoupon(liveCoupon).build();
    }
}
