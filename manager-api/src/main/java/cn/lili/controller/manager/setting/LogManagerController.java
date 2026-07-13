package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.service.SystemLogService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;  
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,日志管理接口
 *
 * @author Chopper
 * @since 2020/11/17 7:56 下午
 */
@Slf4j
@RestController
@Tag(name = "日志管理接口")
@RequestMapping("/manager/setting/log")
public class LogManagerController {
    @Autowired
    private SystemLogService systemLogService;

    @Operation(summary = "分页获取全部")
    @Parameter(name = "type", description = "日志类型", required = false)
    @Parameter(name = "searchKey", description = "搜索关键字", required = true)
    @Parameter(name = "operatorName", description = "操作人名称", required = false)
    @Parameter(name = "searchVo", description = "搜索参数", required = false)
    @Parameter(name = "pageVo", description = "分页参数", required = false)
    @GetMapping("/getAllByPage")
    public ResultMessage<Object> getAllByPage(@RequestParam(required = false) Integer type,
                                              @RequestParam String searchKey,
                                              String operatorName,
                                              SearchVO searchVo,
                                              PageVO pageVo) {
        try {
            return ResultUtil.data(systemLogService.queryLog(null, operatorName, searchKey, searchVo, pageVo));
        } catch (Exception e) {
            log.error("日志获取错误",e);
        }
        return null;
    }


    @Operation(summary = "批量删除")
    @Parameter(name = "ids", description = "日志ID列表", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delByIds(@PathVariable List<String> ids) {
        systemLogService.deleteLog(ids);
        return ResultUtil.success();
    }

    @Operation(summary = "全部删除")
    @DeleteMapping
    public ResultMessage<Object> delAll() {
        systemLogService.flushAll();
        return ResultUtil.success();
    }
}
