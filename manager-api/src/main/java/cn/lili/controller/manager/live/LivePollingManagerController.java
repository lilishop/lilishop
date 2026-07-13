package cn.lili.controller.manager.live;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.live.entity.vos.LivePollingDataVO;
import cn.lili.modules.live.service.LivePollingDataCacheService;
import cn.lili.modules.promotion.entity.dos.MemberCoupon;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collections;
import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Validated
@RestController
@Tag(name = "直播流接口")
@RequestMapping("/manager/live/room/polling")
@RequiredArgsConstructor
public class LivePollingManagerController {

    @Autowired
    private LivePollingDataCacheService livePollingDataCacheService;


    @Operation(summary = "获取直播轮询聚合数据")
    @RequestMapping(value = "/{id}", method = RequestMethod.GET)
    public ResultMessage<LivePollingDataVO> pollingData(@PathVariable("id") String id) {
        // 先读取公共缓存数据（无用户维度），显著降低高并发时的数据库压力。
        LivePollingDataVO globalData = livePollingDataCacheService.getGlobalPollingData(id);
        if (globalData == null) {
            globalData = new LivePollingDataVO();
        }

        List<MemberCoupon> liveCouponReceives = Collections.emptyList();

        // 构造响应副本，避免把用户维度数据写回缓存对象。
        LivePollingDataVO response = LivePollingDataVO.builder()
                .liveDetail(globalData.getLiveDetail())
                .goodsList(globalData.getGoodsList())
                .liveCoupon(globalData.getLiveCoupon())
                .liveCouponReceives(liveCouponReceives)
                .build();
        return ResultUtil.data(response);
    }
}
