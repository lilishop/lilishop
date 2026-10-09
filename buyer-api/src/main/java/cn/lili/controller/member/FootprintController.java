package cn.lili.controller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.FootPrintQueryParams;
import cn.lili.modules.member.service.FootprintService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 买家端,浏览历史接口
 *
 * @author Chopper
 * @since 2020/11/16 10:06 下午
 */
@RestController
@Tag(name = "买家端,浏览历史接口")
@RequestMapping("/buyer/member/footprint")
public class FootprintController {

    /**
     * 客户足迹
     */
    @Autowired
    private FootprintService footprintService;

    @Operation(summary = "分页获取")
    @GetMapping
    public ResultMessage<IPage<GoodsIndex>> getByPage(FootPrintQueryParams params) {
        params.setMemberId(UserContext.getCurrentUser().getId());
        return ResultUtil.data(footprintService.footPrintPage(params));
    }

    @Operation(summary = "根据id删除")
    @Parameter(name = "ids", description = "商品ID", required = true)
    @DeleteMapping("/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@NotNull(message = "商品ID不能为空") @PathVariable("ids") List<String> ids) {
        footprintService.deleteByIds(ids);
        return ResultUtil.success();

    }

    @Operation(summary = "清空足迹")
    @DeleteMapping
    public ResultMessage<Object> deleteAll() {
        footprintService.clean();
        return ResultUtil.success();
    }

    @Operation(summary = "获取当前客户足迹数量")
    @GetMapping("/getFootprintNum")
    public ResultMessage<Object> getFootprintNum() {
        return ResultUtil.data(footprintService.getFootprintNum());
    }


    @GetMapping("/history")
    @Operation(summary = "获取客户的历史足迹")
    public ResultMessage<IPage<GoodsIndex>> getMemberHistory(FootPrintQueryParams params) {
        // 强制使用当前登录用户ID，防止查看他人足迹；不传 memberId 时也不会泄露全平台数据
        params.setMemberId(UserContext.getCurrentUser().getId());
        return ResultUtil.data(footprintService.footPrintPage(params));
    }
}
