/**
 * 作者：Mike
 * 日期：2026-07-31
 */
package cn.lili.modules.goods.util;

import cn.lili.common.properties.CardKeyProperties;
import cn.lili.modules.payment.kit.core.kit.PayKit;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 卡密加解密工具（库内 AES 存储，接口层明文）
 *
 * @author Mike
 * @date 2026-07-31
 */
@Component
public class CardKeySecretUtil {

    @Autowired
    private CardKeyProperties cardKeyProperties;

    /**
     * 将明文卡密 AES 加密后写入 li_card_key.card_secret。
     */
    public String encrypt(String plainSecret) {
        return PayKit.encryptData(plainSecret, cardKeyProperties.getSecret());
    }

    /**
     * 从库内密文解密为明文，供商家授权接口响应使用；禁止在日志中输出结果。
     */
    public String decrypt(String cipherSecret) {
        return PayKit.decryptData(cipherSecret, cardKeyProperties.getSecret());
    }
}
