package cn.lili.controller.seller.common;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 站点基础配置获取
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @since 2022/9/22 17:49
 */
@Slf4j
@RestController
@RequestMapping("/store/common/site")
@Tag(name = "站点基础接口")
public class SiteStoreController {

    @Autowired
    private SettingService settingService;

    @Operation(summary = "获取站点基础信息")
    @GetMapping
    public ResultMessage<Object> baseSetting() {
        return ResultUtil.data(settingService.get(SettingEnum.BASE_SETTING.name()));
    }

    @Operation(summary = "获取主题色配置")
    @GetMapping("/theme")
    public ResultMessage<Object> themeSetting() {
        return ResultUtil.data(settingService.get(SettingEnum.THEME_SETTING.name()));
    }
}
