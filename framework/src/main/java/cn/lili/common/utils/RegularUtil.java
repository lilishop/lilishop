package cn.lili.common.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 用户名验证工具类
 *
 * @author Chopper
 */
public class RegularUtil {


    /**
     * 手机号
     */
    private static final Pattern MOBILE = Pattern.compile("^1[3|4|5|8][0-9]\\d{8}$");

    /**
     * 邮箱
     */
    private static final Pattern EMAIL = Pattern.compile("^[a-zA-Z0-9_.-]+@[a-zA-Z0-9-]+(\\.[a-zA-Z0-9-]+)*\\.[a-zA-Z0-9]{2,6}$");

    //sql正则

    static Pattern sqlPattern = Pattern.compile("(select|update|and|delete|insert|trancate|char|substr|ascii|declare|exec|count|master|into|drop|execute" +
// 可能涉及英文查询参数问题
//                    "|in|not in exists|not exists" +
//                    "|between|not between" +
//                    "|like|not like" +
//                    "|is null|is not null" +
            ")", Pattern.CASE_INSENSITIVE);

    //符号正则
    static Pattern symbolPattern = Pattern.compile("[\\s~·`!！@#￥$%^……&*（()）\\-——\\-_=+【\\[\\]】｛{}｝\\|、\\\\；;：:‘'“”\"，,《<。.》>、/？?]");


    /**
     * 校验手机号
     *
     * @param v
     * @return
     */
    public static boolean mobile(String v) {

        Matcher m = MOBILE.matcher(v);
        if (m.matches()) {
            return true;
        }
        return false;
    }

    //校验邮箱
    public static boolean email(String v) {

        Matcher m = EMAIL.matcher(v);
        if (m.matches()) {
            return true;
        }
        return false;
    }


    /**
     * 搜索参数过滤
     *
     * @param str 字符串
     * @return 过滤后的字符串
     */
    public static String replace(String str) {

        return symbolReplace(sqlReplace(str));
    }

    /**
     * 过滤sql关键字
     *
     * @param str 字符串
     * @return 过滤后的字符串
     */
    public static String sqlReplace(String str) {
        if (StringUtils.isEmpty(str)) {
            return "";
        }
        Matcher sqlMatcher = sqlPattern.matcher(str);
        return sqlMatcher.replaceAll("");
    }

    /**
     * 符号过滤
     *
     * @param str 字符串
     * @return 过滤后的字符串
     */
    public static String symbolReplace(String str) {
        if (StringUtils.isEmpty(str)) {
            return "";
        }
        Matcher symbolMatcher = symbolPattern.matcher(str);
        return symbolMatcher.replaceAll("");
    }

    public static void main(String[] args) {
        System.out.println(replace("selectSELECTINORNOTIN123阿松大asdfa!@#$%^&&*()_+{}[]！？>?").trim());
    }


}
