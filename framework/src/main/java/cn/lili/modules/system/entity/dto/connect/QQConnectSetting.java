package cn.lili.modules.system.entity.dto.connect;

import cn.lili.modules.system.entity.dto.connect.dto.QQConnectSettingItem;
import lombok.Data;

import java.util.List;

/**
 * QQ联合登录设置
 *
 * @author Chopper
 * @since 2020/11/17 7:59 下午
 */
@Data
public class QQConnectSetting {

    /**
     * qq联合登陆配置
     */
    List<QQConnectSettingItem> qqConnectSettingItemList;

}
