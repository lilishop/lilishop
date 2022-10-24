package cn.lili.common.fulu.core.utils;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.security.Security;

/**
 * @Auther: chenYing
 * @Date: 2019/8/27 0027 17:38
 */
public class CardUtil {
  private static final String ALGORITHM = "AES/ECB/PKCS7Padding";


  private CardUtil() {
  }

  public static String cardDecode(String str, byte[] key) {
    byte[] bytes = org.apache.commons.codec.binary.Base64.decodeBase64(str);
    String result = null;
    try {
      Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
      Cipher cipher = Cipher.getInstance(ALGORITHM, "BC");
      SecretKeySpec keySpec = new SecretKeySpec(key, "AES");
      cipher.init(Cipher.DECRYPT_MODE, keySpec);
      byte[] decoded = cipher.doFinal(bytes);
      result = new String(decoded, "UTF-8");
    } catch (Exception e) {
      throw new RuntimeException(e.getMessage(), e);
    }
    return result;
  }

  public static String cardEncode(String str, byte[] key) {
    byte[] result = null;
    try {
      Security.addProvider(new org.bouncycastle.jce.provider.BouncyCastleProvider());
      Cipher cipher = Cipher.getInstance(ALGORITHM, "BC");
      SecretKeySpec keySpec = new SecretKeySpec(key, "AES");
      cipher.init(Cipher.ENCRYPT_MODE, keySpec);
      result = cipher.doFinal(str.getBytes("UTF-8"));
    } catch (Exception e) {
      throw new RuntimeException(e.getMessage(), e);
    }
    return new String(org.apache.commons.codec.binary.Base64.encodeBase64(result));
  }
}
