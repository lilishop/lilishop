package cn.lili.modules.search.utils;

import cn.hutool.core.util.ReflectUtil;
import cn.lili.modules.search.entity.dos.EsGoodsIndex;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Map;

/**
 * @author paulG
 * @since 2021/10/11
 **/
public class EsIndexUtil {

    private static final String IGNORE_FIELD = "serialVersionUID,promotionMap,id,goodsId";

    public static Map<String, Object> getUpdateIndexFieldsMap(EsGoodsIndex queryGoodsIndex, EsGoodsIndex updateGoodsIndex) {
        Map<String, Object> queryFieldsMap = new HashMap<>();
        Map<String, Object> updateFieldsMap = new HashMap<>();

        for (Map.Entry<String, Field> entry : ReflectUtil.getFieldMap(EsGoodsIndex.class).entrySet()) {
            Object queryFieldValue = ReflectUtil.getFieldValue(queryGoodsIndex, entry.getValue());
            Object updateFieldValue = ReflectUtil.getFieldValue(updateGoodsIndex, entry.getValue());
            if (queryFieldValue != null && !IGNORE_FIELD.contains(entry.getKey())) {
                ReflectUtil.setFieldValue(queryFieldsMap, entry.getValue(), queryFieldValue);
            }
            if (updateFieldValue != null && !IGNORE_FIELD.contains(entry.getKey())) {
                ReflectUtil.setFieldValue(updateFieldsMap, entry.getValue(), updateFieldValue);
            }
        }

        return getUpdateIndexFieldsMap(queryFieldsMap, updateFieldsMap);
    }

    public static Map<String, Object> getUpdateIndexFieldsMap(Map<String, Object> queryFieldsMap, Map<String, Object> updateFieldsMap) {
        Map<String, Object> updateIndexMap = new HashMap<>();

        updateIndexMap.put("queryFields", queryFieldsMap);
        updateIndexMap.put("updateFields", updateFieldsMap);
        return updateIndexMap;
    }


}
