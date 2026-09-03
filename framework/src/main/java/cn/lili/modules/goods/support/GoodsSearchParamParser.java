package cn.lili.modules.goods.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import lombok.extern.slf4j.Slf4j;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 商品参数 JSON 统一解析器。同时兼容历史嵌套形态与当前扁平形态。
 * <p>
 * 必须使用两层签名：{@link #parseAll} 只做形态归一，供卖家编辑回填与详情展示；
 * {@link #parseIndexed} 再叠加索引过滤与长度守卫，供搜索与倒排写入。
 * 卖家保存时若误用 {@code parseIndexed}，非索引参数会被静默丢掉。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Slf4j
public final class GoodsSearchParamParser {

    public static final int MAX_PARAM_FIELD_LENGTH = 255;

    private GoodsSearchParamParser() {
    }

    /**
     * 第一层：只做形态归一（嵌套 / 扁平 → DTO 列表），不过滤、不 trim、不去重、不截断。
     * 非法 JSON、{@code null}、空串、缺字段一律返回空集合，不抛异常。
     */
    public static List<GoodsParamsItemDTO> parseAll(String paramsJson) {
        if (CharSequenceUtil.isEmpty(paramsJson)) {
            return Collections.emptyList();
        }
        try {
            JSONArray array = JSON.parseArray(paramsJson);
            if (array == null || array.isEmpty()) {
                return Collections.emptyList();
            }
            List<GoodsParamsItemDTO> items = new ArrayList<>();
            for (int i = 0; i < array.size(); i++) {
                JSONObject element = array.getJSONObject(i);
                if (element == null) {
                    continue;
                }
                JSONArray nested = element.getJSONArray("goodsParamsItemDTOList");
                if (nested != null) {
                    appendItems(items, nested);
                } else {
                    GoodsParamsItemDTO item = element.to(GoodsParamsItemDTO.class);
                    if (item != null) {
                        items.add(item);
                    }
                }
            }
            return items;
        } catch (Exception e) {
            return Collections.emptyList();
        }
    }

    /**
     * 第二层：{@link #parseAll} + isIndex 过滤 + trim + 去重 + 长度守卫，供搜索与倒排使用。
     */
    public static List<GoodsParamsItemDTO> parseIndexed(String paramsJson) {
        List<GoodsParamsItemDTO> all = parseAll(paramsJson);
        if (all.isEmpty()) {
            return Collections.emptyList();
        }
        Map<String, GoodsParamsItemDTO> unique = new LinkedHashMap<>();
        for (GoodsParamsItemDTO item : all) {
            if (!Integer.valueOf(1).equals(item.getIsIndex())) {
                continue;
            }
            String paramName = item.getParamName() == null ? "" : item.getParamName().trim();
            String paramValue = item.getParamValue() == null ? "" : item.getParamValue().trim();
            if (paramName.isEmpty() || paramValue.isEmpty()) {
                continue;
            }
            if (paramName.length() > MAX_PARAM_FIELD_LENGTH || paramValue.length() > MAX_PARAM_FIELD_LENGTH) {
                log.warn("drop goods search param exceeding {} chars, paramNameLength={}, paramValueLength={}",
                        MAX_PARAM_FIELD_LENGTH, paramName.length(), paramValue.length());
                continue;
            }
            item.setParamName(paramName);
            item.setParamValue(paramValue);
            String key = paramName + '\0' + paramValue;
            GoodsParamsItemDTO existing = unique.get(key);
            if (existing == null) {
                unique.put(key, item);
            } else {
                existing.setSort(minSort(existing.getSort(), item.getSort()));
            }
        }
        return new ArrayList<>(unique.values());
    }

    private static void appendItems(List<GoodsParamsItemDTO> items, JSONArray nested) {
        for (int j = 0; j < nested.size(); j++) {
            GoodsParamsItemDTO item = nested.getObject(j, GoodsParamsItemDTO.class);
            if (item != null) {
                items.add(item);
            }
        }
    }

    /**
     * NULL 视为最大：同组内存在非空值时优先保留非空值。
     */
    static Integer minSort(Integer left, Integer right) {
        if (left == null) {
            return right;
        }
        if (right == null) {
            return left;
        }
        return left <= right ? left : right;
    }
}
