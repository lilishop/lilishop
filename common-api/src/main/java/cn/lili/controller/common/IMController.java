package cn.lili.controller.common;


import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ImSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * IM控制器
 *
 * @author Chopper
 * @version v1.0
 * 2021-09-16 15:32
 */
@RestController
@RequestMapping("/common/common/IM")
@Api(tags = "IM 中心")
public class IMController {

    @Autowired
    private SettingService settingService;

    @ApiOperation(value = "获取IM接口前缀")
    @GetMapping
    public ResultMessage<String> getUrl() {
        String imUrl;
        try {
            Setting imSettingVal = settingService.get(SettingEnum.IM_SETTING.name());
            ImSetting imSetting = JSONUtil.toBean(imSettingVal.getSettingValue(), ImSetting.class);
            imUrl = imSetting.getHttpUrl();
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PLATFORM_NOT_SUPPORTED_IM);
        }
        return ResultUtil.data(imUrl);
    }

}
