package cn.lili.controller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.connect.entity.ConnectConfig;
import cn.lili.modules.connect.entity.vo.ConnectConfigForm;
import cn.lili.modules.connect.service.ConnectConfigService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;


/**
 * 管理端,联合登陆配置接口
 *
 * @author Chopper
 * @date 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "管理端,联合登陆配置接口")
@RequestMapping("/manager/connectConfig")
public class ConnectConfigManagerController {

    @Autowired
    private ConnectConfigService connectConfigService;

    @GetMapping(value = "/list")
    @ApiOperation(value = "获取所有联合配置")
    public ResultMessage<List<ConnectConfigForm>> all() {
        return ResultUtil.data(connectConfigService.listForms());
    }

    @GetMapping(value = "/{key}")
    @ApiOperation(value = "查看联合登陆配置详情")
    public ResultMessage<ConnectConfig> get(@PathVariable String key) {
        ConnectConfig connectConfig = connectConfigService.getConfig(key);
        return ResultUtil.data(connectConfig);
    }


    @PutMapping("/{configKey}")
    @ApiOperation(value = "更新联合登陆配置")
    public ResultMessage<ConnectConfig> update(@PathVariable String configKey, ConnectConfig connectConfig) {
        connectConfig.setConfigKey(configKey);
        connectConfigService.saveConfig(connectConfig);
        return ResultUtil.data(connectConfig);
    }

}
