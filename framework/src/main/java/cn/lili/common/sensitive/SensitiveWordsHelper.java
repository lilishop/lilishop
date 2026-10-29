package cn.lili.common.sensitive;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.properties.SystemSettingProperties;
import cn.lili.common.utils.SpringContextUtil;
import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * 敏感词过滤助手
 * 根据敏感词过滤开关设置，对指定场景的内容进行过滤
 *
 * @author Bulbasaur
 * @date 2026-10-29
 */
@Slf4j
public class SensitiveWordsHelper {

    /**
     * 对文本进行敏感词过滤
     * 场景分组关闭过滤时原样返回
     *
     * @param scene 敏感词过滤场景分组
     * @param text  待过滤文本
     * @return 过滤后的文本
     */
    public static String filter(SensitiveFilterScene scene, String text) {
        if (CharSequenceUtil.isBlank(text)) {
            return text;
        }
        //开关判断优先，关闭的场景不读词表
        if (isEnabled(scene) && SensitiveWordsLazyLoader.ensureLoaded()) {
            return SensitiveWordsFilter.filter(text);
        }
        return text;
    }

    /**
     * 对JSON字符串进行敏感词过滤
     * 递归遍历对象和数组，对每个字符串值进行过滤；
     * 场景分组关闭过滤或JSON解析失败时原样返回
     *
     * @param scene 敏感词过滤场景分组
     * @param json  待过滤JSON字符串
     * @return 过滤后的JSON字符串
     */
    public static String filterJson(SensitiveFilterScene scene, String json) {
        //开关判断与词表加载提到递归之外，同一次调用只执行一次
        if (CharSequenceUtil.isBlank(json) || !isEnabled(scene) || !SensitiveWordsLazyLoader.ensureLoaded()) {
            return json;
        }
        try {
            Object parsed = JSONUtil.parse(json);
            filterJsonValue(parsed);
            return JSONUtil.toJsonStr(parsed);
        } catch (Exception e) {
            log.warn("敏感词过滤JSON解析失败，返回原始内容", e);
            return json;
        }
    }

    /**
     * 判断场景分组是否开启敏感词过滤
     *
     * @param scene 敏感词过滤场景分组
     * @return true-开启过滤，false-关闭过滤
     */
    private static boolean isEnabled(SensitiveFilterScene scene) {
        try {
            SystemSettingProperties systemSettingProperties = SpringContextUtil.getBean(SystemSettingProperties.class);
            if (systemSettingProperties != null) {
                return systemSettingProperties.isSensitiveFilterEnabled(scene);
            }
        } catch (Exception ignored) {
        }
        //获取开关失败时默认开启，保持现状
        return true;
    }

    /**
     * 递归遍历JSON，对字符串值进行敏感词过滤
     *
     * @param value JSON节点
     */
    private static void filterJsonValue(Object value) {
        if (value instanceof JSONObject) {
            JSONObject jsonObject = (JSONObject) value;
            for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
                Object entryValue = entry.getValue();
                if (entryValue instanceof String) {
                    jsonObject.set(entry.getKey(), SensitiveWordsFilter.filter((String) entryValue));
                } else {
                    filterJsonValue(entryValue);
                }
            }
        } else if (value instanceof JSONArray) {
            JSONArray jsonArray = (JSONArray) value;
            for (int i = 0; i < jsonArray.size(); i++) {
                Object item = jsonArray.get(i);
                if (item instanceof String) {
                    jsonArray.set(i, SensitiveWordsFilter.filter((String) item));
                } else {
                    filterJsonValue(item);
                }
            }
        }
    }
}
