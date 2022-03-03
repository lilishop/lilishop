package cn.lili.controller.common;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


/**
 * 文件管理管理接口
 *
 * @author Chopper
 * @since 2020/11/26 15:41
 */
@RestController
@Api(tags = "文件管理接口")
@RequestMapping("/common/common/logo")
public class LogoController {

    @Autowired
    private SettingService settingService;

    @ApiOperation(value = "获取logo")
    @GetMapping
    public ResultMessage<Object> getFileList() {
        return ResultUtil.data(settingService.get(SettingEnum.BASE_SETTING.name()));
    }


}
