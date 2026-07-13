package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Collections;

/**
 * 管理端,秒杀活动接口
 *
 * @author paulG
 * @since 2020/8/20
 **/
@RestController
@Tag(name = "管理端,秒杀活动接口")
@RequestMapping("/manager/promotion/seckill")
public class SeckillManagerController {
    @Autowired
    private SeckillService seckillService;
    @Autowired
    private SeckillApplyService seckillApplyService;


    @Operation(summary = "初始化秒杀活动(初始化方法，默认初始化30天内的活动）")
    @GetMapping("/init")
    public void addSeckill() {
        seckillService.init();
    }


    @Operation(summary = "修改秒杀活动")
    @PutMapping(consumes = "application/json", produces = "application/json")
    public ResultMessage<Seckill> updateSeckill(@RequestBody SeckillVO seckillVO) {
        seckillService.updatePromotions(seckillVO);
        return ResultUtil.data(seckillVO);
    }

    @Operation(summary = "通过id获取秒杀活动")
    @Parameter(name = "id", description = "秒杀活动ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Seckill> get(@PathVariable String id) {
        Seckill seckill = seckillService.getById(id);
        return ResultUtil.data(seckill);
    }

    @Operation(summary = "分页查询秒杀活动列表")
    @GetMapping
    public ResultMessage<IPage<Seckill>> getAll(SeckillSearchParams param, PageVO pageVo) {
        return ResultUtil.data(seckillService.pageFindAll(param, pageVo));
    }

    @Operation(summary = "删除一个秒杀活动")
    @Parameter(name = "id", description = "秒杀活动ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> deleteSeckill(@PathVariable String id) {
        seckillService.removePromotions(Collections.singletonList(id));
        return ResultUtil.success();
    }

    @Operation(summary = "操作秒杀活动状态")
    @Parameter(name = "id", description = "秒杀活动ID", required = true)
    @Parameter(name = "startTime", description = "秒杀开始时间", required = true)
    @Parameter(name = "endTime", description = "秒杀结束时间", required = true)
    @PutMapping("/status/{id}")
    public ResultMessage<Object> updateSeckillStatus(@PathVariable String id, Long startTime, Long endTime) {
        seckillService.updateStatus(Collections.singletonList(id), startTime, endTime);
        return ResultUtil.success();
    }

    @Operation(summary = "获取秒杀活动申请列表")
    @GetMapping("/apply")
    public ResultMessage<IPage<SeckillApply>> getSeckillApply(SeckillSearchParams param, PageVO pageVo) {
        IPage<SeckillApply> seckillApply = seckillApplyService.getSeckillApplyPage(param, pageVo);
        return ResultUtil.data(seckillApply);
    }

    @Operation(summary = "删除秒杀活动申请")
    @Parameter(name = "seckillId", description = "秒杀活动ID", required = true)
    @Parameter(name = "id", description = "秒杀活动申请ID", required = true)
    @DeleteMapping("/apply/{seckillId}/{id}")
    public ResultMessage<String> deleteSeckillApply(@PathVariable String seckillId, @PathVariable String id) {
        seckillApplyService.removeSeckillApply(seckillId, id);
        return ResultUtil.success();
    }


}
