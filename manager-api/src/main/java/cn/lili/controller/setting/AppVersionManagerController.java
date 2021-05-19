package cn.lili.controller.setting;

import cn.lili.common.utils.PageUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.AppVersionDO;
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
 * @date 2018-07-04 21:50:52
 */
@RestController
@Api("管理端,app版本控制器")
@RequestMapping("/manager/systems/app/version")
public class AppVersionManagerController {
    @Autowired
    private AppVersionService appVersionService;


    @ApiOperation(value = "查询app升级消息", response = AppVersionDO.class)
    @GetMapping
    @ApiImplicitParams({
            @ApiImplicitParam(name = "type", value = "APP类型", required = true, dataType = "type", paramType = "query")
    })
    public ResultMessage<IPage<AppVersionDO>> getByPage(PageVO page, String type) {
        return ResultUtil.data(this.appVersionService.page(PageUtil.initPage(page),
                new QueryWrapper<AppVersionDO>().eq(StringUtils.isNotEmpty(type), "type", type).orderByDesc("create_time")));
    }


    @ApiOperation(value = "添加app版本信息", response = AppVersionDO.class)
    @PostMapping
    public ResultMessage<Boolean> add(@Valid AppVersionDO appVersionDO) {
        return ResultUtil.data(this.appVersionService.save(appVersionDO));
    }

    @PutMapping(value = "/{id}")
    @ApiOperation(value = "修改app版本信息", response = AppVersionDO.class)
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "主键", required = true, dataType = "String", paramType = "path")
    })
    public ResultMessage<Boolean> edit(@Valid AppVersionDO appVersionDO, @PathVariable String id) {
        if (appVersionService.getById(id) != null) {
            return ResultUtil.data(this.appVersionService.updateById(appVersionDO));
        }
        return ResultUtil.data(false);
    }

    @DeleteMapping(value = "/{id}")
    @ApiOperation(value = "删除app版本")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "要删除的app版本主键", required = true, dataType = "String", paramType = "path")
    })
    public ResultMessage<Boolean> delete(@PathVariable String id) {
        if (appVersionService.getById(id) != null) {
            return ResultUtil.data(this.appVersionService.removeById(id));
        }
        return ResultUtil.data(true);
    }

}
