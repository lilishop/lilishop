package cn.lili.modules.connect.entity.dto;

import lombok.Data;

/**
 * WechatMPLoginParams
 *
 * @author Chopper
 * @version v1.0
 * 2021-02-19 16:34
 */
@Data
public class WechatMPLoginParams {
    /**
     * uuid 用户uuid
     * code 微信返回code 用于与微信交互获取openid 等信息
     * encryptedData 微信返回加密信息
     * iv 微信返回
     * image 微信头像
     * nickname 微信用户昵称
     */
    private String uuid, code, encryptedData, iv, image, nickName;
}
