package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.vos.SeckillGoodsVO;
import cn.lili.modules.promotion.entity.vos.SeckillTimelineVO;
import cn.lili.modules.promotion.service.SeckillApplyService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;


/**
 * 买家端,秒杀活动接口
 *
 * @author paulG
 * @since 2020/11/17 2:30 下午
 */
@Api(tags = "买家端,秒杀活动接口")
@RestController
@RequestMapping("/buyer/promotion/seckill")
public class SeckillBuyerController {

    /**
     * 秒杀活动
     */
    @Autowired
    private SeckillApplyService seckillApplyService;

    @ApiOperation(value = "获取当天秒杀活动信息")
    @GetMapping
    public ResultMessage<List<SeckillTimelineVO>> getSeckillTime() {
        return ResultUtil.data(seckillApplyService.getSeckillTimeline());
    }

    @ApiOperation(value = "获取某个时刻的秒杀活动商品信息")
    @GetMapping("/{timeline}")
    public ResultMessage<List<SeckillGoodsVO>> getSeckillGoods(@PathVariable Integer timeline) {
        return ResultUtil.data(seckillApplyService.getSeckillGoods(timeline));
    }

}
