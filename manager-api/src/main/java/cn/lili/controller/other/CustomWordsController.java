package cn.lili.controller.other;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.permission.SettingKeys;
import cn.lili.modules.search.service.CustomWordsService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.service.SettingService;
import io.swagger.annotations.Api;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.nio.charset.StandardCharsets;

/**
 * 管理端,自定义分词接口
 *
 * @author paulG
 * @since 2020/10/16
 **/
@Slf4j
@RestController
@Api(tags = "管理端,自定义分词接口")
@RequestMapping("/manager/custom-words")
public class CustomWordsController {

    /**
     * 分词
     */
    @Autowired
    private CustomWordsService customWordsService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;

    @GetMapping
    public String getCustomWords(String secretKey) {
        if (StringUtils.isEmpty(secretKey)) {
            return "";
        }
        Setting setting = settingService.get(SettingKeys.ES_SIGN.name());
        if (setting == null || StringUtils.isEmpty(setting.getSettingValue())) {
            return "";
        }

        if (!setting.getSettingValue().equals(secretKey)) {
            throw new ServiceException(ResultCode.CUSTOM_WORDS_SECRET_KEY_ERROR);
        }

        String res = customWordsService.deploy();
        try {
            return new String(res.getBytes(), StandardCharsets.UTF_8);
        } catch (Exception e) {
            log.error("获取分词错误",e);
        }
        return "";
    }

}
