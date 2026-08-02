package cn.lili.modules.goods.util;

import org.springframework.stereotype.Component;

/**
 * 卡密读写归一化工具（库内明文存储，与接口层一致）。
 *
 * @author Mike
 * @date 2026-07-31
 */
@Component
public class CardKeySecretUtil {

    /**
     * 写入库前的卡密值（当前为明文直存，保留方法便于调用点统一）。
     */
    public String encrypt(String plainSecret) {
        return plainSecret;
    }

    /**
     * 从库中读出供接口返回的卡密值（当前为明文直读）。
     */
    public String decrypt(String storedSecret) {
        return storedSecret;
    }
}
