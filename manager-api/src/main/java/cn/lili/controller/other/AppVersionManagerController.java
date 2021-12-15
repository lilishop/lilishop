package cn.lili.controller.other;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.AppVersion;
import cn.lili.modules.system.service.AppVersionService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;

/**
 * 管理端,app版本控制器
 *
 * @author Chopper
 * @since 2018-07-04 21:50:52
 */
@RestController
@Api("管理端,app版本控制器")
@RequestMapping("/manager/other/appVersion")
public class AppVersionManagerController {
    @Autowired
    private AppVersionService appVersionService;


    @ApiOperation(value = "查询app升级消息", response = AppVersion.class)
    @GetMapping
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "APP类型", required = true, dataType = "type", paramType = "query")
    })
    public ResultMessage<IPage<AppVersion>> getByPage(PageVO page, String type) {
        return ResultUtil.data(this.appVersionService.page(PageUtil.initPage(page),
                new QueryWrapper<AppVersion>().eq(StringUtils.isNotEmpty(type), "type", type).orderByDesc("create_time")));
    }


    @ApiOperation(value = "添加app版本信息", response = AppVersion.class)
    @PostMapping
    public ResultMessage<Object> add(@Valid AppVersion appVersion) {

        if(this.appVersionService.checkAppVersion(appVersion)){
            if(this.appVersionService.save(appVersion)){
                return ResultUtil.success();
            }
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @PutMapping(value = "/{id}")
    @ApiOperation(value = "修改app版本信息", response = AppVersion.class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "主键", required = true, dataType = "String", paramType = "path")
    })
    public ResultMessage<Object> edit(@Valid AppVersion appVersion, @PathVariable String id) {

        if(this.appVersionService.checkAppVersion(appVersion)){
            if(this.appVersionService.updateById(appVersion)){
                return ResultUtil.success();
            }
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除app版本")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "要删除的app版本主键", required = true, dataType = "String", paramType = "path")
    })
    public ResultMessage<Boolean> delete(@PathVariable String id) {
        if(this.appVersionService.removeById(id)){
            return ResultUtil.success();
        }
        throw new ServiceException(ResultCode.ERROR);
    }

}
