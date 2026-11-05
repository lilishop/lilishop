package cn.lili.controller.manager.setting;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingConfigService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,系统设置接口
 *
 * @author Chopper
 * @since 2020/11/26 15:53
 */
@RestController
@Tag(name = "管理端,系统设置接口")
@RequestMapping("/manager/setting/setting")
public class SettingManagerController {
    @Autowired
    private SettingConfigService settingConfigService;

    @DemoSite
    @SystemLogPoint(description = "更新系统配置", customerLog = "'配置项: [' + #key + ']'")
    @Operation(summary = "更新配置")
    @Parameter(name = "key", description = "配置key", required = true)
    @Parameter(name = "configValue", description = "配置值", required = true)
    @PutMapping( "/put/{key}")
    public ResultMessage saveConfig(@PathVariable String key, @RequestBody String configValue) {
        settingConfigService.saveConfig(SettingEnum.valueOf(key), configValue);
        return ResultUtil.success();
    }


    @DemoSite
    @Operation(summary = "查看配置")
    @Parameter(name = "key", description = "配置key", required = true)
    @GetMapping( "/get/{key}")
    public ResultMessage settingGet(@PathVariable String key) {
        return ResultUtil.data(settingConfigService.getConfig(SettingEnum.valueOf(key)));
    }
}
