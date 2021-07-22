package cn.lili.common.utils;

import java.util.Random;
import java.util.UUID;

/**
 * 通用工具
 * @author Chopper
 */
public class CommonUtil {

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

        Random random = new Random();
        int num = random.nextInt(999999);
        //不足六位前面补0
        String str = String.format("%06d", num);
        return str;
    }

}
