package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * 公众号通知设置
 * <p>
 * 会员在小程序「消息通知」页点击「公众号通知」后跳转到 {@link #articleUrl} 对应的公众号文章，
 * 手动关注公众号；公众号把关注事件回调到服务端后，通过 UnionID 匹配会员并记录公众号 openid。
 * 公众号的 appId / appSecret 仍复用 WECHAT_CONNECT 中 clientType = H5 的配置。
 *
 * @author Bulbasaur
 * @since 2026-11-02
 */
@Data
public class WechatOfficialAccountSetting implements Serializable {

    private static final long serialVersionUID = 4127839245563019812L;

    /**
     * 是否开启公众号通知入口（关闭后小程序端不展示入口）
     */
    private Boolean enabled;

    /**
     * 公众号名称（前端展示用）
     */
    private String accountName;

    /**
     * 引导关注的公众号文章链接
     */
    private String articleUrl;

    /**
     * 公众号「服务器配置」中的令牌 Token，用于回调签名校验
     */
    private String serverToken;

    /**
     * 公众号「服务器配置」中的消息加解密密钥 EncodingAESKey（明文模式可不填）
     */
    private String encodingAesKey;

    /**
     * 关注公众号后的自动回复内容
     */
    private String subscribeReply;
}
