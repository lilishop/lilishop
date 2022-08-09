package cn.lili.controller.common;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 滑块验证码接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@Slf4j
@RestController
@RequestMapping("/common/common/site")
@Api(tags = "系统基础接口")
public class SiteController {

    @Autowired
    private SettingService settingService;

    @ApiOperation(value = "获取系统基础信息")
    @GetMapping
    public ResultMessage<Object> getFileList() {
        return ResultUtil.data(settingService.get(SettingEnum.BASE_SETTING.name()));
    }
}
