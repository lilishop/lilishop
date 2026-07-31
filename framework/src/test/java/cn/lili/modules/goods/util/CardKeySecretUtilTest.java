/**
 * 作者：Mike
 * 日期：2026-07-31
 */
package cn.lili.modules.goods.util;

import cn.lili.common.properties.CardKeyProperties;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * CardKeySecretUtil 加解密往返测试。
 *
 * @author Mike
 * @date 2026-07-31
 */
class CardKeySecretUtilTest {

    @Test
    void encryptAndDecryptRoundTrip() {
        CardKeyProperties properties = new CardKeyProperties();
        properties.setSecret("unit-test-card-key-secret");
        CardKeySecretUtil util = new CardKeySecretUtil();
        ReflectionTestUtils.setField(util, "cardKeyProperties", properties);

        String plain = "GAME-SECRET-12345";
        String cipher = util.encrypt(plain);

        assertThat(cipher).isNotEqualTo(plain);
        assertThat(util.decrypt(cipher)).isEqualTo(plain);
    }
}
