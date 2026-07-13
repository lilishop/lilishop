package cn.lili.controller.seller.settings;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.permission.service.SystemLogService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Objects;


/**
 * 店铺端,日志管理接口
 *
 * @author Chopper
 * @since 2020/11/22 14:23
 */
@RestController
@Tag(name = "店铺端,日志管理接口")
@RequestMapping("/store/settings/log")
public class LogStoreController {
    @Autowired
    private SystemLogService systemLogService;

    @Operation(description = "分页获取全部")
    @Parameter(name = "type", description = "日志类型")
    @Parameter(name = "key", description = "搜索关键词")
    @Parameter(name = "operatorName", description = "操作人名称")
    @Parameter(name = "searchVo", description = "搜索VO")
    @Parameter(name = "pageVo", description = "分页VO")
    @GetMapping("/getAllByPage")
    public ResultMessage<Object> getAllByPage(@RequestParam(required = false) Integer type,
                                              @RequestParam String key,
                                              String operatorName,
                                              SearchVO searchVo,
                                              PageVO pageVo) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        return ResultUtil.data(systemLogService.queryLog(storeId, operatorName, key, searchVo, pageVo));
    }
}
