package cn.lili.controller.manager.wxchannels;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wxchannels.entity.dto.WxChannelsSetting;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

@RestController
@Tag(name = "管理端,微信视频号设置接口")
@RequestMapping("/manager/wxchannels/setting")
public class WxChannelsSettingManagerController {

    @Autowired
    private SettingService settingService;

    @GetMapping
    @Operation(summary = "获取视频号小店配置")
    public ResultMessage<WxChannelsSetting> get() {
        Setting s = settingService.get(SettingEnum.WX_CHANNELS.name());
        if (s == null || s.getSettingValue() == null) {
            return ResultUtil.data(new WxChannelsSetting());
        }
        return ResultUtil.data(JSONUtil.toBean(s.getSettingValue(), WxChannelsSetting.class));
    }

    @PostMapping
    @Operation(summary = "保存视频号小店配置")
    public ResultMessage<Object> save(@RequestBody WxChannelsSetting setting) {
        Setting s = new Setting();
        s.setId(SettingEnum.WX_CHANNELS.name());
        s.setSettingValue(JSONUtil.toJsonStr(setting));
        settingService.saveUpdate(s);
        return ResultUtil.success();
    }
}
