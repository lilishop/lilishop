package cn.lili.common.utils;

import lombok.extern.slf4j.Slf4j;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 用户名验证工具类
 * @author Chopper
 */
public class RegularUtil {


    /**
     * 手机号
     */
    private static final Pattern mobile = Pattern.compile("^1[3|4|5|8][0-9]\\d{8}$");

    /**
     * 邮箱
     */
    private static final Pattern email = Pattern.compile("^[a-zA-Z0-9_.-]+@[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*\\.[a-zA-Z0-9]{2,6}$");

    public static boolean Mobile(String v){

        Matcher m = mobile.matcher(v);
        if(m.matches()){
            return true;
        }
        return false;
    }

    public static boolean Email(String v){

        Matcher m = email.matcher(v);
        if(m.matches()){
            return true;
        }
        return false;
    }
}
