package cn.lili.common.utils;

import org.springframework.beans.BeanUtils;

import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * 对象属性复制
 * @author Chopper
 */
public class BeanUtil {

    /**
     * 复制属性
     * @param objectFrom
     * @param objectTo
     */
    public static void copyProperties(Object objectFrom,Object objectTo){
        BeanUtils.copyProperties(objectFrom, objectTo);
    }


    /**
     * 获取属性名数组
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
            if (superFields[i].getName().equals("id")) {
                continue;
            }
            fieldNames[index] = superFields[i].getName();
            index++;
        }
        return fieldNames;
    }

    /* 根据属性名获取属性值
     * */
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
}
