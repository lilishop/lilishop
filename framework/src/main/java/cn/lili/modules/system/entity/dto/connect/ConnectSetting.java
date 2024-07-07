package cn.lili.modules.system.entity.dto.connect;

import cn.lili.modules.system.entity.dto.connect.dto.WechatConnectSettingItem;
import lombok.Data;

import java.util.List;

/**
 * 登录设置
 *
 * @author Chopper
 * @since 2020/11/17 8:00 下午
 */
@Data
public class ConnectSetting {


    /**
     * 买家PC端域名
     */
    private String pc;

    /**
     * 买家WAP端域名
     */
    private String wap;

    /**
     * 回调域名
     */
    private String callbackUrl;
}
