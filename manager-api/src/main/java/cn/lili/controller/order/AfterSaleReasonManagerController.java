package cn.lili.controller.order;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.order.aftersale.entity.dos.AfterSaleReason;
import cn.lili.modules.order.aftersale.service.AfterSaleReasonService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * 管理端,售后原因接口
 *
 * @author Bulbasaur
 * @since 2021/1/6 14:11
 */
@RestController
@RequestMapping("/manager/order/afterSaleReason")
@Api(tags = "管理端,售后原因接口")
public class AfterSaleReasonManagerController {

    /**
     * 售后原因
     */
    @Autowired
    private AfterSaleReasonService afterSaleReasonService;

    @ApiOperation(value = "查看售后原因")
    @ApiImplicitParam(name = "id", value = "售后原因ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<AfterSaleReason> get(@PathVariable String id) {

        return ResultUtil.data(afterSaleReasonService.getById(id));
    }

    @ApiOperation(value = "分页获取售后原因")
    @GetMapping(value = "/getByPage")
    @ApiImplicitParam(name = "serviceType", value = "售后类型", required = true, dataType = "String", paramType = "query")
    public ResultMessage<IPage<AfterSaleReason>> getByPage(PageVO page, @RequestParam String serviceType) {
        return ResultUtil.data(afterSaleReasonService.page(PageUtil.initPage(page),new QueryWrapper<AfterSaleReason>().eq("service_Type", serviceType)));
    }

    @ApiOperation(value = "添加售后原因")
    @PostMapping
    public ResultMessage<AfterSaleReason> save(@Valid AfterSaleReason afterSaleReason) {
        afterSaleReasonService.save(afterSaleReason);
        return ResultUtil.data(afterSaleReason);
    }

    @ApiOperation(value = "修改售后原因")
    @ApiImplicitParam(name = "id", value = "关键词ID", required = true, dataType = "String", paramType = "path")
    @PutMapping("update/{id}")
    public ResultMessage<AfterSaleReason> update(@Valid AfterSaleReason afterSaleReason, @PathVariable("id") String id) {
        afterSaleReason.setId(id);
        return ResultUtil.data(afterSaleReasonService.editAfterSaleReason(afterSaleReason));
    }

    @ApiOperation(value = "删除售后原因")
    @ApiImplicitParam(name = "id", value = "售后原因ID", required = true, dataType = "String", paramType = "path")
    @DeleteMapping(value = "/delByIds/{id}")
    public ResultMessage<Object> delAllByIds(@PathVariable String id) {
        afterSaleReasonService.removeById(id);
        return ResultUtil.success();
    }
}
