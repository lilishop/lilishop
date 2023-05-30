package cn.lili.common.utils;

import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

/**
 * 通用工具
 * @author Chopper
 */
public class CommonUtil {

    public static final String BASE_NUMBER = "0123456789";

    /**
     * 以UUID重命名
     * @param fileName 文件名称
     * @return 格式化名称
     */
    public static String rename(String fileName) {
        String extName = fileName.substring(fileName.lastIndexOf("."));
        return UUID.randomUUID().toString().replace("-", "") + extName;
    }


    /**
     * 随机6位数生成
     */
    public static String getRandomNum() {
        StringBuilder sb = new StringBuilder(6);
        for (int i = 0; i < 6; i++) {
            int num = ThreadLocalRandom.current().nextInt(BASE_NUMBER.length());
            sb.append(BASE_NUMBER.charAt(num));
        }
        return sb.toString();
    }

}
