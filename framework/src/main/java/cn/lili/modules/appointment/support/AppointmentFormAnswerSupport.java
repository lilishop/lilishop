package cn.lili.modules.appointment.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

/**
 * 预约补充信息：买家答案按字段 ID 存成 map，并带上字段快照便于展示。
 *
 * <pre>
 * {
 *   "systemFormId": "...",
 *   "formSubmitMode": "PER_ORDER",
 *   "fields": [{ "fieldKey": "80943e0a-...", "fieldLabel": "身份证号", "fieldType": "TEXT" }],
 *   "answers": [{ "80943e0a-...": "310..." }]
 * }
 * </pre>
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
public final class AppointmentFormAnswerSupport {

    public static final String MODE_PER_ORDER = "PER_ORDER";
    public static final String MODE_PER_APPOINTMENT = "PER_APPOINTMENT";

    private AppointmentFormAnswerSupport() {
    }

    /**
     * 解析表单字段；没有 field_key 时按序号生成兼容键。
     *
     * @param schemaJson 系统表单 schema / 发品快照
     * @return 可填写字段（不含富文本）
     */
    public static List<JSONObject> parseFillableFields(String schemaJson) {
        List<JSONObject> all = parseFields(schemaJson);
        List<JSONObject> fillable = new ArrayList<>();
        for (JSONObject field : all) {
            if (!isRichText(field.getStr("fieldType"))) {
                fillable.add(field);
            }
        }
        return fillable;
    }

    /**
     * 解析全部字段（含富文本）。
     *
     * @param schemaJson 表单 JSON
     * @return 字段列表
     */
    public static List<JSONObject> parseFields(String schemaJson) {
        List<JSONObject> fields = new ArrayList<>();
        if (CharSequenceUtil.isBlank(schemaJson)) {
            return fields;
        }
        JSONArray array = parseSchemaArray(schemaJson);
        if (array == null) {
            return fields;
        }
        for (int i = 0; i < array.size(); i++) {
            Object item = array.get(i);
            if (!(item instanceof JSONObject)) {
                continue;
            }
            JSONObject field = (JSONObject) item;
            String key = firstNonBlank(field, "field_key", "fieldKey", "id", "key");
            if (CharSequenceUtil.isBlank(key)) {
                key = "field_" + i;
            }
            String label = firstNonBlank(field, "field_label", "fieldLabel", "label", "title");
            String type = firstNonBlank(field, "field_type", "fieldType", "type");
            JSONObject normalized = new JSONObject();
            normalized.set("fieldKey", key);
            normalized.set("fieldLabel", CharSequenceUtil.blankToDefault(label, key));
            normalized.set("fieldType", CharSequenceUtil.blankToDefault(type, "TEXT"));
            normalized.set("required", isRequired(field));
            normalized.set("hint", firstNonBlank(field, "hint_text", "hintText", "hint", "placeholder"));
            if (isRichText(normalized.getStr("fieldType"))) {
                normalized.set("richTextContent", firstNonBlank(field,
                        "rich_text_content", "richTextContent", "content"));
            }
            fields.add(normalized);
        }
        return fields;
    }

    /**
     * 将买家提交的答案规范成落库 JSON；按字段 ID 存值。
     *
     * @param rawJson        买家提交
     * @param schemaJson     表单快照
     * @param systemFormId   表单 ID
     * @param formSubmitMode 提交粒度
     * @param quantity       购买件数
     * @return 规范化 JSON；未关联表单时返回 null
     */
    public static String normalize(String rawJson,
                                   String schemaJson,
                                   String systemFormId,
                                   String formSubmitMode,
                                   int quantity) {
        if (CharSequenceUtil.isBlank(systemFormId) && CollUtil.isEmpty(parseFillableFields(schemaJson))) {
            return null;
        }
        List<JSONObject> fields = parseFields(schemaJson);
        List<JSONObject> fillable = parseFillableFields(schemaJson);
        int groupCount = resolveGroupCount(formSubmitMode, quantity);
        List<Map<String, Object>> answerGroups = extractAnswerGroups(rawJson, fillable);
        if (answerGroups.size() != groupCount) {
            throw new ServiceException(ResultCode.APPOINTMENT_FORM_INCOMPLETE);
        }
        for (JSONObject field : fillable) {
            if (!Boolean.TRUE.equals(field.get("required"))) {
                continue;
            }
            String fieldKey = field.getStr("fieldKey");
            for (Map<String, Object> group : answerGroups) {
                if (isBlankValue(group.get(fieldKey))) {
                    throw new ServiceException(ResultCode.APPOINTMENT_FORM_INCOMPLETE);
                }
            }
        }
        JSONObject stored = new JSONObject();
        stored.set("systemFormId", systemFormId);
        stored.set("formSubmitMode", CharSequenceUtil.blankToDefault(formSubmitMode, MODE_PER_APPOINTMENT));
        JSONArray fieldSnap = new JSONArray();
        for (JSONObject field : fields) {
            JSONObject snap = new JSONObject();
            snap.set("fieldKey", field.getStr("fieldKey"));
            snap.set("fieldLabel", field.getStr("fieldLabel"));
            snap.set("fieldType", field.getStr("fieldType"));
            fieldSnap.add(snap);
        }
        stored.set("fields", fieldSnap);
        stored.set("answers", answerGroups);
        return stored.toString();
    }

    /**
     * 给 schema 补 field_key，写入商品快照。
     *
     * @param schemaJson 原始 schema
     * @return 带稳定 field_key 的 JSON
     */
    public static String snapshotSchema(String schemaJson) {
        if (CharSequenceUtil.isBlank(schemaJson)) {
            return schemaJson;
        }
        JSONArray array = parseSchemaArray(schemaJson);
        if (array == null) {
            return schemaJson;
        }
        for (Object item : array) {
            if (!(item instanceof JSONObject)) {
                continue;
            }
            JSONObject field = (JSONObject) item;
            String key = firstNonBlank(field, "field_key", "fieldKey", "id", "key");
            if (CharSequenceUtil.isBlank(key)) {
                key = UUID.randomUUID().toString();
            }
            if (CharSequenceUtil.isBlank(field.getStr("field_key"))) {
                field.set("field_key", key);
            }
            String type = firstNonBlank(field, "field_type", "fieldType", "type");
            if (CharSequenceUtil.isNotBlank(type) && CharSequenceUtil.isBlank(field.getStr("field_type"))) {
                field.set("field_type", type.toUpperCase().replace("-", "_"));
            }
            String label = firstNonBlank(field, "field_label", "fieldLabel", "label", "title");
            if (CharSequenceUtil.isNotBlank(label) && CharSequenceUtil.isBlank(field.getStr("field_label"))) {
                field.set("field_label", label);
            }
        }
        return array.toString();
    }

    private static int resolveGroupCount(String formSubmitMode, int quantity) {
        int qty = Math.max(quantity, 1);
        if (MODE_PER_ORDER.equalsIgnoreCase(formSubmitMode)) {
            return 1;
        }
        return qty;
    }

    private static List<Map<String, Object>> extractAnswerGroups(String rawJson, List<JSONObject> fillable) {
        List<Map<String, Object>> groups = new ArrayList<>();
        if (CharSequenceUtil.isBlank(rawJson)) {
            return groups;
        }
        Object parsed;
        try {
            parsed = JSONUtil.parse(rawJson);
        } catch (Exception e) {
            throw new ServiceException(ResultCode.APPOINTMENT_FORM_INCOMPLETE);
        }
        if (parsed instanceof JSONObject) {
            JSONObject object = (JSONObject) parsed;
            if (object.containsKey("answers")) {
                groups.addAll(fromAnswersNode(object.get("answers"), fillable));
            } else {
                groups.add(toValueMap(object, fillable));
            }
        } else if (parsed instanceof JSONArray) {
            groups.addAll(fromAnswersNode(parsed, fillable));
        }
        return groups;
    }

    private static List<Map<String, Object>> fromAnswersNode(Object node, List<JSONObject> fillable) {
        List<Map<String, Object>> groups = new ArrayList<>();
        if (!(node instanceof JSONArray)) {
            if (node instanceof JSONObject) {
                groups.add(toValueMap((JSONObject) node, fillable));
            }
            return groups;
        }
        JSONArray array = (JSONArray) node;
        for (Object item : array) {
            if (item instanceof JSONObject) {
                groups.add(toValueMap((JSONObject) item, fillable));
            } else if (item instanceof JSONArray) {
                groups.add(fromFieldArray((JSONArray) item, fillable));
            } else if (item instanceof Map) {
                groups.add(toValueMap(JSONUtil.parseObj(item), fillable));
            }
        }
        return groups;
    }

    private static Map<String, Object> fromFieldArray(JSONArray array, List<JSONObject> fillable) {
        Map<String, Object> values = new LinkedHashMap<>();
        for (Object item : array) {
            if (!(item instanceof JSONObject)) {
                continue;
            }
            JSONObject object = (JSONObject) item;
            String key = firstNonBlank(object, "field_key", "fieldKey", "id", "key");
            Object value = object.containsKey("value") ? object.get("value") : object.get("val");
            if (CharSequenceUtil.isNotBlank(key)) {
                values.put(key, value);
            }
        }
        return restrictToFields(values, fillable);
    }

    private static Map<String, Object> toValueMap(JSONObject object, List<JSONObject> fillable) {
        Map<String, Object> values = new LinkedHashMap<>();
        for (String key : object.keySet()) {
            if ("systemFormId".equals(key) || "formSubmitMode".equals(key)
                    || "fields".equals(key) || "answers".equals(key)) {
                continue;
            }
            Object value = object.get(key);
            if (value instanceof JSONObject && ((JSONObject) value).containsKey("value")) {
                values.put(key, ((JSONObject) value).get("value"));
            } else {
                values.put(key, value);
            }
        }
        return restrictToFields(values, fillable);
    }

    private static Map<String, Object> restrictToFields(Map<String, Object> values, List<JSONObject> fillable) {
        Map<String, Object> restricted = new LinkedHashMap<>();
        if (CollUtil.isEmpty(fillable)) {
            restricted.putAll(values);
            return restricted;
        }
        for (JSONObject field : fillable) {
            String key = field.getStr("fieldKey");
            if (values.containsKey(key)) {
                restricted.put(key, values.get(key));
            }
        }
        return restricted;
    }

    private static JSONArray parseSchemaArray(String schemaJson) {
        try {
            Object parsed = JSONUtil.parse(schemaJson);
            if (parsed instanceof JSONArray) {
                return (JSONArray) parsed;
            }
            if (parsed instanceof JSONObject) {
                Object fields = ((JSONObject) parsed).get("fields");
                if (fields instanceof JSONArray) {
                    return (JSONArray) fields;
                }
            }
        } catch (Exception ignored) {
            return null;
        }
        return null;
    }

    private static boolean isRichText(String type) {
        if (CharSequenceUtil.isBlank(type)) {
            return false;
        }
        return "RICH_TEXT".equalsIgnoreCase(type)
                || "richtext".equalsIgnoreCase(type.replace("-", "_"));
    }

    private static boolean isRequired(JSONObject field) {
        Object required = field.get("required");
        if (required instanceof Boolean) {
            return (Boolean) required;
        }
        return Boolean.parseBoolean(String.valueOf(required));
    }

    private static boolean isBlankValue(Object value) {
        if (value == null) {
            return true;
        }
        if (value instanceof String) {
            return CharSequenceUtil.isBlank((String) value);
        }
        if (value instanceof JSONArray) {
            return ((JSONArray) value).isEmpty();
        }
        return false;
    }

    private static String firstNonBlank(JSONObject object, String... keys) {
        for (String key : keys) {
            Object value = object.get(key);
            if (value != null && CharSequenceUtil.isNotBlank(String.valueOf(value))) {
                return String.valueOf(value);
            }
        }
        return null;
    }
}
