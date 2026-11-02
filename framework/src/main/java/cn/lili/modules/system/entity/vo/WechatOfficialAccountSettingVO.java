package cn.lili.modules.system.entity.vo;

import lombok.Data;

import java.io.Serializable;

/**
 * 买家端 - 公众号通知配置（仅包含可公开的字段，不返回 Token / EncodingAESKey 等敏感配置）
 *
 * @author Bulbasaur
 * @since 2026-11-02
 */
@Data
public class WechatOfficialAccountSettingVO implements Serializable {

    private static final long serialVersionUID = -2261917306120378345L;

    /**
     * 是否开启公众号通知入口
     */
    private Boolean enabled;

    /**
     * 公众号名称
     */
    private String accountName;

    /**
     * 引导关注的公众号文章链接
     */
    private String articleUrl;

    /**
     * 当前会员是否已绑定公众号（已关注且已记录公众号 openid）
     */
    private Boolean bound;
}
