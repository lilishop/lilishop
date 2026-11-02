package cn.lili.controller.message;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.WechatOfficialAccountSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.entity.vo.WechatOfficialAccountSettingVO;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 买家端,公众号通知接口
 *
 * @author Bulbasaur
 * @since 2026-11-02
 */
@RestController
@Tag(name = "买家端,公众号通知接口")
@RequestMapping("/buyer/message/wechatOfficialAccount")
public class WechatOfficialAccountBuyerController {

    @Autowired
    private SettingService settingService;

    @Autowired
    private ConnectService connectService;

    @Operation(summary = "获取公众号通知配置及当前会员绑定状态")
    @GetMapping
    public ResultMessage<WechatOfficialAccountSettingVO> get() {
        WechatOfficialAccountSettingVO vo = new WechatOfficialAccountSettingVO();
        Setting setting = settingService.get(SettingEnum.WECHAT_OFFICIAL_ACCOUNT_SETTING.name());
        WechatOfficialAccountSetting oaSetting = setting == null || setting.getSettingValue() == null
                ? new WechatOfficialAccountSetting()
                : JSONUtil.toBean(setting.getSettingValue(), WechatOfficialAccountSetting.class);

        boolean enabled = Boolean.TRUE.equals(oaSetting.getEnabled()) && oaSetting.getArticleUrl() != null;
        vo.setEnabled(enabled);
        vo.setAccountName(oaSetting.getAccountName());
        vo.setArticleUrl(enabled ? oaSetting.getArticleUrl() : null);

        AuthUser authUser = UserContext.getCurrentUser();
        boolean bound = false;
        if (authUser != null) {
            bound = connectService.count(new LambdaQueryWrapper<Connect>()
                    .eq(Connect::getUserId, authUser.getId())
                    .eq(Connect::getUnionType, SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name())) > 0;
        }
        vo.setBound(bound);
        return ResultUtil.data(vo);
    }
}
