package cn.lili.controller.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;
import java.util.List;


/**
 * 买家端,浏览历史接口
 *
 * @author Chopper
 * @date: 2020/11/16 10:06 下午
 */
@RestController
@Api(tags = "买家端,浏览历史接口")
@RequestMapping("/buyer/footprint")
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class FootprintController {

    /**
     * 会员足迹
     */
    private final FootprintService footprintService;

    @ApiOperation(value = "分页获取")
    @GetMapping
    public ResultMessage<List<EsGoodsIndex>> getByPage(PageVO page) {
        return ResultUtil.data(footprintService.footPrintPage(page));
    }

    @ApiOperation(value = "根据id删除")
    @ApiImplicitParam(name = "ids", value = "商品ID", required = true, allowMultiple = true, dataType = "String", paramType = "path")
    @DeleteMapping(value = "/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@NotNull(message = "商品ID不能为空") @PathVariable("ids") List ids) {
        if (footprintService.deleteByIds(ids)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @ApiOperation(value = "清空足迹")
    @DeleteMapping
    public ResultMessage<Object> deleteAll() {
        if (footprintService.clean()) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @ApiOperation(value = "获取当前会员足迹数量")
    @GetMapping(value = "/getFootprintNum")
    public ResultMessage<Object> getFootprintNum() {
        return ResultUtil.data(footprintService.getFootprintNum());
    }

}
