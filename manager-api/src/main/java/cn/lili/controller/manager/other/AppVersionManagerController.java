package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.AppVersion;
import cn.lili.modules.system.service.AppVersionService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

/**
 * 管理端,app版本控制器
 *
 * @author Chopper
 * @since 2018-07-04 21:50:52
 */
@RestController
@Tag(name = "管理端,app版本控制器")
@RequestMapping("/manager/other/appVersion")
public class AppVersionManagerController {
    @Autowired
    private AppVersionService appVersionService;


    @Operation(summary = "查询app升级消息")
    @Parameter(name = "page", description = "分页参数")
    @Parameter(name = "type", description = "APP类型", required = true)
    @GetMapping
    public ResultMessage<IPage<AppVersion>> getByPage(PageVO page, String type) {
        return ResultUtil.data(this.appVersionService.pageByType(type, page));
    }


    @Operation(summary = "添加app版本信息")
    @Parameter(name = "appVersion", description = "app版本信息", required = true)
    @PostMapping
    public ResultMessage<Object> add(@Valid AppVersion appVersion) {

        if(this.appVersionService.checkAppVersion(appVersion)){
            if(this.appVersionService.save(appVersion)){
                return ResultUtil.success();
            }
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @PutMapping("/{id}")
    @Operation(summary = "修改app版本信息")
    @Parameter(name = "id", description = "主键", required = true)
    public ResultMessage<Object> edit(@Valid AppVersion appVersion, 
            @PathVariable String id) {
        if(this.appVersionService.checkAppVersion(appVersion)){
            if(this.appVersionService.updateById(appVersion)){
                return ResultUtil.success();
            }
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @DeleteMapping("/{id}")
    @Operation(summary = "删除app版本")
    @Parameter(name = "id", description = "主键", required = true)
    public ResultMessage<Boolean> delete(
            @PathVariable String id) {
        if(this.appVersionService.removeById(id)){
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }

}
