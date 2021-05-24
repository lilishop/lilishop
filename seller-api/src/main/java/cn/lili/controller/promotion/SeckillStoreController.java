package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.entity.vos.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Arrays;
import java.util.List;

/**
 * 店铺端,秒杀活动接口
 *
 * @author paulG
 * @date 2020/8/26
 **/
@RestController
@Api(tags = "店铺端,秒杀活动接口")
@RequestMapping("/store/promotion/seckill")
public class SeckillStoreController {
    @Autowired
    private SeckillService seckillService;
    @Autowired
    private SeckillApplyService seckillApplyService;

    @GetMapping
    @ApiOperation(value = "获取秒杀活动列表")
    public ResultMessage<IPage<SeckillVO>> getSeckillPage(SeckillSearchParams queryParam, PageVO pageVo) {
        IPage<SeckillVO> seckillPage = seckillService.getSeckillByPageFromMongo(queryParam, pageVo);
        return ResultUtil.data(seckillPage);
    }

    @GetMapping("/apply")
    @ApiOperation(value = "获取秒杀活动申请列表")
    public ResultMessage<IPage<SeckillApply>> getSeckillApplyPage(SeckillSearchParams queryParam, PageVO pageVo) {
        String storeId = UserContext.getCurrentUser().getStoreId();
        queryParam.setStoreId(storeId);
        IPage<SeckillApply> seckillPage = seckillApplyService.getSeckillApplyFromMongo(queryParam, pageVo);
        return ResultUtil.data(seckillPage);
    }

    @GetMapping("/{seckillId}")
    @ApiOperation(value = "获取秒杀活动")
    public ResultMessage<Seckill> getSeckill(@PathVariable String seckillId) {
        return ResultUtil.data(seckillService.getSeckillByIdFromMongo(seckillId));
    }

    @GetMapping("/apply/{seckillApplyId}")
    @ApiOperation(value = "获取秒杀活动申请")
    public ResultMessage<SeckillApply> getSeckillApply(@PathVariable String seckillApplyId) {
        return ResultUtil.data(seckillApplyService.getById(seckillApplyId));
    }

    @PostMapping(path = "/apply/{seckillId}", consumes = "application/json", produces = "application/json")
    @ApiOperation(value = "添加秒杀活动申请")
    public ResultMessage<String> addSeckillApply(@PathVariable String seckillId, @RequestBody List<SeckillApplyVO> applyVos) {
        String storeId = UserContext.getCurrentUser().getStoreId();
        seckillApplyService.addSeckillApply(seckillId, storeId, applyVos);
        return ResultUtil.success();
    }

    @DeleteMapping("/apply/{seckillId}/{ids}")
    @ApiOperation(value = "删除秒杀活动申请")
    public ResultMessage<String> deleteSeckillApply(@PathVariable("seckillId") String seckillId, @PathVariable("ids") String ids) {
        String[] idsSplit = ids.split(",");
        seckillApplyService.removeSeckillApplyByIds(seckillId, Arrays.asList(idsSplit));
        return ResultUtil.success();
    }


}
