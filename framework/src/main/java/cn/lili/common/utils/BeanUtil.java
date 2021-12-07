package cn.lili.common.utils;

import org.springframework.beans.BeanUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * 对象属性复制
 *
 * @author Chopper
 */
public class BeanUtil {

    /**
     * 复制属性
     *
     * @param objectFrom 源自对象
     * @param objectTo   复制给对象
     */
    public static void copyProperties(Object objectFrom, Object objectTo) {
        BeanUtils.copyProperties(objectFrom, objectTo);
    }


    /**
     * 获取属性名数组
     *
     * @param o 获取字段的对象
     * @return 返回各个字段
     */
    public static String[] getFiledName(Object o) {
        Field[] fields = o.getClass().getDeclaredFields();
        Field[] superFields = o.getClass().getSuperclass().getDeclaredFields();
        String[] fieldNames = new String[fields.length + superFields.length];
        int index = 0;
        for (int i = 0; i < fields.length; i++) {
            fieldNames[index] = fields[i].getName();
            index++;
        }
        for (int i = 0; i < superFields.length; i++) {
            if ("id".equals(superFields[i].getName())) {
                continue;
            }
            fieldNames[index] = superFields[i].getName();
            index++;
        }
        return fieldNames;
    }

    /**
     * 根据属性名获取属性值
     *
     * @param fieldName 属性名
     * @param o         对象
     * @return 属性值
     */
    public static Object getFieldValueByName(String fieldName, Object o) {
        try {
            String firstLetter = fieldName.substring(0, 1).toUpperCase();
            String getter = "get" + firstLetter + fieldName.substring(1);
            Method method = o.getClass().getMethod(getter, new Class[]{});
            Object value = method.invoke(o, new Object[]{});
            return value;
        } catch (Exception e) {
            return null;
        }
    }


    /**
     * 将对象转换为key value
     * A=a&B=b&C=c 格式
     *
     * @param object 对象
     * @return 格式化结果
     */
    public static String formatKeyValuePair(Object object) {
        //准备接受的字符串
        StringBuilder stringBuffer = new StringBuilder();
        //获取对象字段
        String[] fieldNames = BeanUtil.getFiledName(object);
        //遍历所有属性
        for (int j = 0; j < fieldNames.length; j++) {
            //不是第一个并且不是最后一个，拼接&
            if (j != 0) {
                stringBuffer.append("&");
            }
            //获取属性的名字
            String key = fieldNames[j];
            //获取值
            Object value = BeanUtil.getFieldValueByName(key, object);
            assert value != null;
            stringBuffer.append(key).append("=").append(value.toString());
        }
        return stringBuffer.toString();
    }

    /**
     * key value键值对 转换为 对象
     * A=a&B=b&C=c 格式 转换为对象
     *
     * @param str 对象字符串
     * @param t   范型
     * @param <T> 范型
     * @return 格式化结果
     */
    public static <T> T formatKeyValuePair(String str, T t) {
        //填写对参数键值对
        String[] params = str.split("&");

        //获取对象字段
        String[] fieldNames = BeanUtil.getFiledName(t);

        try {
            //循环每个参数
            for (String param : params) {
                String[] keyValues = param.split("=");
                for (int i = 0; i < fieldNames.length; i++) {
                    if (fieldNames[i].equals(keyValues[0])) {
                        Field f = t.getClass().getDeclaredField(fieldNames[i]);
                        f.setAccessible(true);
                        //长度为2 才转换，否则不转
                        if (keyValues.length == 2) {
                            f.set(t, keyValues[1]);
                        }
                    }
                }

            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return t;
    }

}
