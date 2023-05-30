package cn.lili.controller.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.AppVersion;
import cn.lili.modules.system.service.AppVersionService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,APP版本
 *
 * @author Bulbasaur
 * @since 2021/5/21 11:15 上午
 */
@RestController
@Api(tags = "买家端,APP版本")
@RequestMapping("/buyer/other/appVersion")
public class AppVersionBuyerController {

    @Autowired
    private AppVersionService appVersionService;


    @ApiOperation(value = "获取版本号")
    @ApiImplicitParam(name = "appType", value = "app类型", required = true, paramType = "path")
    @GetMapping("/{appType}")
    public ResultMessage<Object> getAppVersion(@PathVariable String appType) {
        return ResultUtil.data(appVersionService.getAppVersion(appType));
    }

    @ApiOperation(value = "获取版本号列表")
    @ApiImplicitParam(name = "appType", value = "app类型", required = true, paramType = "path")
    @GetMapping("/appVersion/{appType}")
    public ResultMessage<IPage<AppVersion>> appVersion(@PathVariable String appType, PageVO pageVO) {

        IPage<AppVersion> page = appVersionService.page(PageUtil.initPage(pageVO), new LambdaQueryWrapper<AppVersion>().eq(AppVersion::getType, appType));
        return ResultUtil.data(page);
    }
}
