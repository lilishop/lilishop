package cn.lili.controller.promotion;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.search.KanJiaActivityLogQuery;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityGoodsParams;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityQuery;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsListVO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityGoodsVO;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityVO;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityLogService;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 买家端,砍价活动商品
 *
 * @author qiuqiu
 * @date 2021/7/12
 **/
@RestController
@Api(tags = "买家端,砍价商品接口")
@RequestMapping("/buyer/promotion/kanjiaGoods")
public class KanjiaGoodsActivityBuyerController {

    /**
     * 砍价活动商品
     */
    @Autowired
    private KanjiaActivityGoodsService kanJiaActivityGoodsService;
    /**
     * 帮砍记录
     */
    @Autowired
    private KanjiaActivityLogService kanJiaActivityLogService;
    /**
     * 砍价活动
     */
    @Autowired
    private KanjiaActivityService kanJiaActivityService;

    @GetMapping
    @ApiOperation(value = "分页获取砍价商品")
    public ResultMessage<IPage<KanjiaActivityGoodsListVO>> kanjiaActivityGoodsPage(KanjiaActivityGoodsParams kanjiaActivityGoodsParams, PageVO page) {
        // 会员端查询到的肯定是已经开始的活动商品
        kanjiaActivityGoodsParams.setPromotionStatus(PromotionsStatusEnum.START.name());
//        kanjiaActivityGoodsParams.setStartTime(System.currentTimeMillis());
//        kanjiaActivityGoodsParams.setEndTime(System.currentTimeMillis());
        return ResultUtil.data(kanJiaActivityGoodsService.kanjiaGoodsVOPage(kanjiaActivityGoodsParams, page));
    }

    @GetMapping("/{id}")
    @ApiOperation(value = "获取砍价活动商品")
    @ApiImplicitParam(name = "id", value = "砍价活动商品ID", required = true, paramType = "path")
    public ResultMessage<KanjiaActivityGoodsVO> getKanjiaActivityGoods(@PathVariable String id) {
        return ResultUtil.data(kanJiaActivityGoodsService.getKanJiaGoodsVO(id));
    }

    @GetMapping("/getKanjiaActivity/logs")
    @ApiOperation(value = "分页获取砍价活动-帮砍记录")
    public ResultMessage<IPage<KanjiaActivityLog>> getKanjiaActivityLog(KanJiaActivityLogQuery kanJiaActivityLogQuery, PageVO page) {
        return ResultUtil.data(kanJiaActivityLogService.getForPage(kanJiaActivityLogQuery, page));
    }

    @PostMapping("/getKanjiaActivity")
    @ApiOperation(value = "获取砍价活动")
    public ResultMessage<KanjiaActivityVO> getKanJiaActivity(KanjiaActivitySearchParams kanjiaActivitySearchParams) {
        //如果是非被邀请关系则填写会员ID
        if (CharSequenceUtil.isEmpty(kanjiaActivitySearchParams.getKanjiaActivityId())) {
            kanjiaActivitySearchParams.setMemberId(UserContext.getCurrentUser().getId());
        }
        return ResultUtil.data(kanJiaActivityService.getKanjiaActivityVO(kanjiaActivitySearchParams));
    }

    @PostMapping
    @ApiImplicitParam(name = "id", value = "砍价活动商品ID", required = true, paramType = "path")
    @ApiOperation(value = "发起砍价活动")
    public ResultMessage<KanjiaActivityLog> launchKanJiaActivity(String id) {
        KanjiaActivityLog kanjiaActivityLog = kanJiaActivityService.add(id);
        return ResultUtil.data(kanjiaActivityLog);
    }

    @PostMapping("/help/{kanjiaActivityId}")
    @ApiImplicitParam(name = "kanJiaActivityId", value = "砍价活动ID", required = true, paramType = "path")
    @ApiOperation(value = "帮砍一刀")
    public ResultMessage<KanjiaActivityLog> helpKanJia(@PathVariable String kanjiaActivityId) {
        KanjiaActivityLog kanjiaActivityLog = kanJiaActivityService.helpKanJia(kanjiaActivityId);
        return ResultUtil.data(kanjiaActivityLog);
    }

    @GetMapping("/kanjiaActivity/mine/")
    @ApiOperation(value = "分页获取已参与的砍价活动")
    public ResultMessage<IPage<KanjiaActivity>> getPointsGoodsPage(KanjiaActivityQuery kanjiaActivityQuery, PageVO page) {
        // 会员端查询到的肯定是已经开始的活动商品
        kanjiaActivityQuery.setMemberId(UserContext.getCurrentUser().getId());
        IPage<KanjiaActivity> kanjiaActivity = kanJiaActivityService.getForPage(kanjiaActivityQuery, page);
        return ResultUtil.data(kanjiaActivity);
    }

}
