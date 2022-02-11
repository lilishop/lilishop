package cn.lili.modules.system.entity.dto.connect.dto;


import lombok.Data;

/**
 * QQ联合登录具体配置
 *
 * @author Chopper
 * @since 2020/11/17 7:59 下午
 */
@Data
public class QQConnectSettingItem {

    private String clientType;

    private String appId;

    private String appKey;


}
