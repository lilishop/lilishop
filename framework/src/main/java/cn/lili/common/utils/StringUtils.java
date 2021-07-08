package cn.lili.common.utils;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.StrUtil;

import java.beans.BeanInfo;
import java.beans.Introspector;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 字串工具类
 *
 * @author pikachu
 */
public class StringUtils extends StrUtil {

    /**
     * MD5加密方法
     *
     * @param str String
     * @return String
     */
    public static String md5(String str) {
        MessageDigest messageDigest = null;
        try {
            messageDigest = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException ex) {
            ex.printStackTrace();
            return null;
        }
        byte[] resultByte = messageDigest.digest(str.getBytes());
        StringBuffer result = new StringBuffer();
        for (int i = 0; i < resultByte.length; ++i) {
            int v = 0xFF & resultByte[i];
            if (v < 16) {
                result.append("0");
            }
            result.append(Integer.toHexString(v));
        }
        return result.toString();
    }

    /**
     * 将object转为数字
     *
     * @param obj     需要转object的对象
     * @param checked 如果为true格式不正确抛出异常
     * @return
     */
    public static int toInt(Object obj, boolean checked) {
        int value = 0;
        if (obj == null) {
            return 0;
        }
        try {
            value = Convert.toInt(obj.toString());
        } catch (Exception ex) {
            if (checked) {
                throw new RuntimeException("整型数字格式不正确");
            } else {
                return 0;
            }
        }
        return value;
    }

    /**
     * 将一个字串转为long，如果无空，则返回默认值
     *
     * @param str          要转换的数字字串
     * @param defaultValue 默认值
     * @return
     */
    public static Long toLong(String str, Long defaultValue) {
        Long value = defaultValue;
        if (str == null || "".equals(str)) {
            return defaultValue;
        }
        try {
            value = Long.parseLong(str);
        } catch (Exception ex) {
            return defaultValue;
        }
        return value;
    }

    /**
     * 将一个object转为double 如果object 为 null 则返回0；
     *
     * @param obj     需要转成Double的对象
     * @param checked 如果为true格式不正确抛出异常
     * @return
     */
    public static Double toDouble(Object obj, boolean checked) {
        Double value = 0d;
        if (obj == null) {
            if (checked) {
                throw new RuntimeException("数字格式不正确");
            } else {
                return 0D;
            }
        }
        try {
            value = Double.valueOf(obj.toString());
        } catch (Exception ex) {
            if (checked) {
                throw new RuntimeException("数字格式不正确");
            } else {
                return 0D;
            }
        }
        return value;
    }

    /**
     * 将一个字串转为Double，如果无空，则返回默认值
     *
     * @param str          要转换的数字字串
     * @param defaultValue 默认值
     * @return
     */
    public static Double toDouble(String str, Double defaultValue) {
        Double value = defaultValue;
        if (str == null || "".equals(str)) {
            return 0d;
        }
        try {
            value = Double.valueOf(str);
        } catch (Exception ex) {
            ex.printStackTrace();
            value = defaultValue;
        }
        return value;
    }

    /**
     * 获取随机数
     *
     * @param n 随机次数
     * @return
     */
    public static String getRandStr(int n) {
        Random random = new Random();
        String sRand = "";
        for (int i = 0; i < n; i++) {
            String rand = String.valueOf(random.nextInt(10));
            sRand += rand;
        }
        return sRand;
    }


    /**
     * 判断一个数组是否为空，并且长度大于0
     *
     * @param list
     * @return true 不空/false 空
     */
    public static boolean isNotEmpty(List list) {

        return list != null && list.size() > 0;
    }

    /**
     * 切个字符串，如果超出长度则切割
     *
     * @param var
     * @param size
     * @return
     */
    public static String subStringLength(String var, Integer size) {
        if (var.length() > size) {
            return var.substring(0, (size - 4)).toString() + "...";
        }
        return var;
    }

    /**
     * 对象转map
     *
     * @param obj
     * @return
     * @throws Exception
     */
    public static Map<String, Object> objectToMap(Object obj) throws Exception {
        if (obj == null) {
            return null;
        }
        Map<String, Object> map = new HashMap<String, Object>(16);

        BeanInfo beanInfo = Introspector.getBeanInfo(obj.getClass());
        PropertyDescriptor[] propertyDescriptors = beanInfo.getPropertyDescriptors();
        for (PropertyDescriptor property : propertyDescriptors) {
            String key = property.getName();
            if (key.compareToIgnoreCase("class") == 0) {
                continue;
            }
            Method getter = property.getReadMethod();
            Object value = getter != null ? getter.invoke(obj) : null;
            map.put(key, value);
        }

        return map;
    }


    /**
     * 驼峰法转下划线
     */
    public static String camel2Underline(String str) {

        if (StrUtil.isBlank(str)) {
            return "";
        }
        if (str.length() == 1) {
            return str.toLowerCase();
        }
        StringBuffer sb = new StringBuffer();
        for (int i = 1; i < str.length(); i++) {
            if (Character.isUpperCase(str.charAt(i))) {
                sb.append("_" + Character.toLowerCase(str.charAt(i)));
            } else {
                sb.append(str.charAt(i));
            }
        }
        return (str.charAt(0) + sb.toString()).toLowerCase();
    }

    /**
     * 如果给定字符串{@code str}中不包含{@code appendStr}，则在{@code str}后追加{@code appendStr}；
     * 如果已包含{@code appendStr}，则在{@code str}后追加{@code otherwise}
     *
     * @param str       给定的字符串
     * @param appendStr 需要追加的内容
     * @param otherwise 当{@code appendStr}不满足时追加到{@code str}后的内容
     * @return 追加后的字符串
     */
    public static String appendIfNotContain(String str, String appendStr, String otherwise) {
        if (isEmpty(str) || isEmpty(appendStr)) {
            return str;
        }
        if (str.contains(appendStr)) {
            return str.concat(otherwise);
        }
        return str.concat(appendStr);
    }

    /**
     * 过滤特殊字符串
     *
     * @param str
     * @return
     */
    public static String filterSpecialChart(String str) {
        String regEx = "[`~!@#$%^&*()+=|{}':;',\\[\\].<>/?~！@#￥%……&*（）——+|{}【】‘；：”“’。，、？]";
        Pattern p = Pattern.compile(regEx);
        Matcher m = p.matcher(str);
        return m.replaceAll("").trim();
    }
}


