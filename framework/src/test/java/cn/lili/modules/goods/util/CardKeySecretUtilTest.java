package cn.lili.modules.goods.util;

import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * CardKeySecretUtil 明文读写测试。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeySecretUtilTest {

    /**
     * 验证卡密加密后解密能够还原原始明文。
     */
    @Test
    void encryptAndDecryptReturnPlaintext() {
        CardKeySecretUtil util = new CardKeySecretUtil();
        String plain = "GAME-SECRET-12345";

        assertThat(util.encrypt(plain)).isEqualTo(plain);
        assertThat(util.decrypt(plain)).isEqualTo(plain);
    }
}
