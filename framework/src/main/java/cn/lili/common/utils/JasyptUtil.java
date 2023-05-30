package cn.lili.common.utils;

import org.jasypt.encryption.pbe.PooledPBEStringEncryptor;
import org.jasypt.encryption.pbe.config.SimpleStringPBEConfig;


/**
 * 加密解密
 * @author Chopper
 */
public class JasyptUtil {

    /**
     * Jasypt生成加密结果
     * @param password 配置文件中设定的加密密码 jasypt.encryptor.password
     * @param value 待加密值
     * @return 加密字符串
     */
    public static String encryptPwd(String password, String value){
        PooledPBEStringEncryptor encryptor = new PooledPBEStringEncryptor();
        encryptor.setConfig(encryptor(password));
        return encryptor.encrypt(value);
    }
    /**
     * 解密
     * @param password 配置文件中设定的加密密码 jasypt.encryptor.password
     * @param value 待解密密文
     * @return 揭秘字符串
     */
    public static String decryptPwd(String password, String value){
        PooledPBEStringEncryptor encryptor = new PooledPBEStringEncryptor();
        encryptor.setConfig(encryptor(password));
        encryptor.decrypt(value);
        return encryptor.decrypt(value);
    }

    /**
     * 加密处理
     * @param password 密码
     * @return 加密字符串
     */
    public static SimpleStringPBEConfig encryptor(String password){
        SimpleStringPBEConfig config = new SimpleStringPBEConfig();
        config.setPassword(password);
        config.setAlgorithm("PBEWITHHMACSHA512ANDAES_256");
        config.setKeyObtentionIterations("1000");
        config.setPoolSize(1);
        config.setProviderName("SunJCE");
        config.setSaltGeneratorClassName("org.jasypt.salt.RandomSaltGenerator");
        config.setIvGeneratorClassName("org.jasypt.iv.RandomIvGenerator");
        config.setStringOutputType("base64");
        return config;
    }

    public static void main(String[] args){

        //加密 若修改了第一个参数加密password记得在配置文件同步修改
        System.out.println(encryptPwd("jasypt.encryptor.password","123456"));
        //解密
        System.out.println(decryptPwd("jasypt.encryptor.password","PYVnAYh+j5C3jkMV1d+myj6JzDaUk7pcfTWUaYsvQdEVkuvIVf7Y0mOU9XkffxT8"));
    }
}
