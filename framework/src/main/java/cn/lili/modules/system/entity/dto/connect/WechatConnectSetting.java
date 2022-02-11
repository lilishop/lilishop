package cn.lili.modules.system.entity.dto.connect;

import cn.lili.modules.system.entity.dto.connect.dto.WechatConnectSettingItem;
import lombok.Data;

import java.util.List;

/**
 * 微信设置
 *
 * @author Chopper
 * @since 2020/11/17 8:00 下午
 */
@Data
public class WechatConnectSetting {


    /**
     * 微信联合登陆配置
     */
    List<WechatConnectSettingItem> wechatConnectSettingItems;

}
