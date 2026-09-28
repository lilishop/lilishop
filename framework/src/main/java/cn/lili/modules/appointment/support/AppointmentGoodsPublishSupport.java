package cn.lili.modules.appointment.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.NumberUtil;
import cn.lili.modules.appointment.entity.dos.AppointmentGoodsExt;
import cn.lili.modules.appointment.entity.dos.AppointmentSlotTemplate;
import cn.lili.modules.appointment.entity.dto.AppointmentGoodsExtSaveDTO;
import cn.lili.modules.appointment.entity.dto.AppointmentSlotTemplateDTO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 * 预约商品发品参数与持久化结构互转
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
public final class AppointmentGoodsPublishSupport {

    private AppointmentGoodsPublishSupport() {
    }

    /**
     * 将发品提交的预约设置、时段模板、SKU 可约数量转换为保存 DTO
     *
     * @param goodsId        商品ID
     * @param settings       预约设置
     * @param slotTemplate   时段模板
     * @param skuList        发品 SKU 列表
     * @param persistedSkus  已落库 SKU
     * @return 预约扩展保存参数
     */
    @SuppressWarnings("unchecked")
    public static AppointmentGoodsExtSaveDTO toSaveDto(String goodsId,
                                                       Map<String, Object> settings,
                                                       Map<String, Object> slotTemplate,
                                                       List<Map<String, Object>> skuList,
                                                       List<GoodsSku> persistedSkus) {
        AppointmentGoodsExtSaveDTO dto = new AppointmentGoodsExtSaveDTO();
        dto.setGoodsId(goodsId);
        if (settings != null) {
            applySettings(dto, settings);
        }
        if (slotTemplate != null) {
            dto.setSlotDivisionMode(stringVal(slotTemplate.get("slotDivisionMode")));
            dto.setServiceDurationMinutes(intVal(slotTemplate.get("serviceDurationMinutes")));
            dto.setAutoStartTime(stringVal(slotTemplate.get("autoStartTime")));
            dto.setAutoEndTime(stringVal(slotTemplate.get("autoEndTime")));
        }
        dto.setSlotTemplates(buildSlotTemplates(slotTemplate, skuList, persistedSkus));
        return dto;
    }

    /**
     * 将预约扩展转为商家编辑回显设置
     *
     * @param ext 预约扩展
     * @return 发品页预约设置
     */
    public static Map<String, Object> toSettingsMap(AppointmentGoodsExt ext) {
        if (ext == null) {
            return null;
        }
        Map<String, Object> map = new LinkedHashMap<>();
        if (CharSequenceUtil.isNotBlank(ext.getServiceModes())) {
            map.put("serviceModes", Arrays.asList(ext.getServiceModes().split(",")));
        }
        map.put("showRemainingQty", ext.getShowRemainingQty());
        map.put("bookableDateMode", ext.getBookableDateMode());
        map.put("bookableDays", ext.getBookingDateRangeDays());
        if (ext.getCustomDateStart() != null) {
            map.put("bookableCustomStartDate", DateUtil.formatDate(ext.getCustomDateStart()));
        }
        if (ext.getCustomDateEnd() != null) {
            map.put("bookableCustomEndDate", DateUtil.formatDate(ext.getCustomDateEnd()));
        }
        if (CharSequenceUtil.isNotBlank(ext.getCustomWeekdays())) {
            map.put("bookableWeekdays", Arrays.stream(ext.getCustomWeekdays().split(","))
                    .filter(CharSequenceUtil::isNotBlank)
                    .map(s -> NumberUtil.parseInt(s.trim()))
                    .collect(Collectors.toList()));
        }
        map.put("advanceMode", ext.getAdvanceBookingMode());
        map.put("advanceHours", ext.getAdvanceBookingHours());
        map.put("cancelMode", ext.getCancelMode());
        map.put("cancelAheadHours", ext.getCancelHours());
        map.put("formSubmitMode", ext.getFormSubmitMode());
        map.put("systemFormId", ext.getSystemFormId());
        return map;
    }

    /**
     * 将预约扩展与时段模板转为商家编辑回显时段划分
     *
     * @param ext       预约扩展
     * @param templates 时段模板
     * @return 发品页时段模板
     */
    public static Map<String, Object> toSlotTemplateMap(AppointmentGoodsExt ext, List<AppointmentSlotTemplate> templates) {
        Map<String, Object> map = new LinkedHashMap<>();
        if (ext != null) {
            map.put("slotDivisionMode", ext.getSlotDivisionMode());
            map.put("serviceDurationMinutes", ext.getServiceDurationMinutes());
            map.put("autoStartTime", ext.getAutoStartTime());
            map.put("autoEndTime", ext.getAutoEndTime());
        }
        if (CollUtil.isEmpty(templates)) {
            map.put("slots", Collections.emptyList());
            return map;
        }
        LinkedHashMap<String, Map<String, Object>> unique = new LinkedHashMap<>();
        for (AppointmentSlotTemplate template : templates) {
            String start = template.getSlotStartTime() == null ? "" : template.getSlotStartTime();
            String end = template.getSlotEndTime() == null ? "" : template.getSlotEndTime();
            String key = start + "-" + end;
            Map<String, Object> slot = new LinkedHashMap<>();
            slot.put("startTime", start);
            slot.put("endTime", end);
            slot.put("slotEnabled", template.getEnabled() == null || template.getEnabled());
            unique.putIfAbsent(key, slot);
        }
        map.put("slots", new ArrayList<>(unique.values()));
        return map;
    }

    /**
     * 将各 SKU 时段可约数量挂到商品 VO
     *
     * @param goodsVO   商品 VO
     * @param templates 时段模板
     */
    public static void attachSkuSlotCapacities(GoodsVO goodsVO, List<AppointmentSlotTemplate> templates) {
        if (goodsVO == null || CollUtil.isEmpty(goodsVO.getSkuList()) || CollUtil.isEmpty(templates)) {
            return;
        }
        Map<String, List<AppointmentSlotTemplate>> bySku = templates.stream()
                .filter(t -> CharSequenceUtil.isNotBlank(t.getSkuId()))
                .collect(Collectors.groupingBy(AppointmentSlotTemplate::getSkuId));
        for (GoodsSkuVO sku : goodsVO.getSkuList()) {
            List<AppointmentSlotTemplate> skuTemplates = bySku.getOrDefault(sku.getId(), Collections.emptyList());
            List<Map<String, Object>> caps = skuTemplates.stream()
                    .sorted(Comparator.comparing(t -> t.getSortOrder() == null ? 0 : t.getSortOrder()))
                    .map(t -> {
                        Map<String, Object> cap = new LinkedHashMap<>();
                        cap.put("startTime", t.getSlotStartTime());
                        cap.put("endTime", t.getSlotEndTime());
                        cap.put("slotCapacity", t.getCapacity());
                        return cap;
                    })
                    .collect(Collectors.toList());
            sku.setSlotCapacities(caps);
        }
    }

    @SuppressWarnings("unchecked")
    private static void applySettings(AppointmentGoodsExtSaveDTO dto, Map<String, Object> settings) {
        Object modes = settings.get("serviceModes");
        if (modes instanceof Collection<?> collection) {
            dto.setServiceModes(collection.stream().map(String::valueOf).collect(Collectors.joining(",")));
        } else if (modes != null) {
            dto.setServiceModes(String.valueOf(modes));
        }
        dto.setShowRemainingQty(boolVal(settings.get("showRemainingQty"), true));
        dto.setBookableDateMode(stringVal(settings.get("bookableDateMode")));
        dto.setCustomDateStart(dateVal(settings.get("bookableCustomStartDate")));
        dto.setCustomDateEnd(dateVal(settings.get("bookableCustomEndDate")));
        Object weekdays = settings.get("bookableWeekdays");
        if (weekdays instanceof Collection<?> collection) {
            dto.setCustomWeekdays(collection.stream().map(String::valueOf).collect(Collectors.joining(",")));
        }
        dto.setBookingDateRangeDays(intVal(settings.get("bookableDays")));
        dto.setAdvanceBookingMode(stringVal(settings.get("advanceMode")));
        dto.setAdvanceBookingHours(intVal(settings.get("advanceHours")));
        dto.setCancelMode(stringVal(settings.get("cancelMode")));
        dto.setCancelHours(intVal(settings.get("cancelAheadHours")));
        dto.setFormSubmitMode(stringVal(settings.get("formSubmitMode")));
        Object formId = settings.get("systemFormId");
        if (formId != null && CharSequenceUtil.isNotBlank(String.valueOf(formId))) {
            dto.setSystemFormId(String.valueOf(formId));
        }
    }

    @SuppressWarnings("unchecked")
    private static List<AppointmentSlotTemplateDTO> buildSlotTemplates(Map<String, Object> slotTemplate,
                                                                       List<Map<String, Object>> skuList,
                                                                       List<GoodsSku> persistedSkus) {
        if (slotTemplate == null || CollUtil.isEmpty(persistedSkus)) {
            return Collections.emptyList();
        }
        List<Map<String, Object>> slots = (List<Map<String, Object>>) slotTemplate.get("slots");
        if (CollUtil.isEmpty(slots)) {
            return Collections.emptyList();
        }
        List<AppointmentSlotTemplateDTO> result = new ArrayList<>();
        int sort = 0;
        for (GoodsSku sku : persistedSkus) {
            Map<String, Object> skuMap = matchSkuMap(sku, skuList, persistedSkus.indexOf(sku));
            List<Map<String, Object>> capacities = skuMap == null ? null
                    : (List<Map<String, Object>>) skuMap.get("slotCapacities");
            for (Map<String, Object> slot : slots) {
                if (Boolean.FALSE.equals(slot.get("slotEnabled"))) {
                    continue;
                }
                String start = stringVal(slot.get("startTime"));
                String end = stringVal(slot.get("endTime"));
                AppointmentSlotTemplateDTO template = new AppointmentSlotTemplateDTO();
                template.setSkuId(sku.getId());
                template.setSlotStartTime(start);
                template.setSlotEndTime(end);
                template.setEnabled(true);
                template.setCapacity(resolveCapacity(capacities, start, end));
                template.setSortOrder(sort++);
                result.add(template);
            }
        }
        return result;
    }

    private static Integer resolveCapacity(List<Map<String, Object>> capacities, String start, String end) {
        if (CollUtil.isEmpty(capacities)) {
            return 0;
        }
        for (Map<String, Object> cap : capacities) {
            if (start.equals(stringVal(cap.get("startTime"))) && end.equals(stringVal(cap.get("endTime")))) {
                return intVal(cap.get("slotCapacity"));
            }
        }
        return 0;
    }

    private static Map<String, Object> matchSkuMap(GoodsSku sku, List<Map<String, Object>> skuList, int index) {
        if (CollUtil.isEmpty(skuList)) {
            return null;
        }
        for (Map<String, Object> map : skuList) {
            Object id = map.get("id");
            if (id != null && sku.getId().equals(String.valueOf(id))) {
                return map;
            }
            Object sn = map.get("sn");
            if (sn != null && sku.getSn() != null && sku.getSn().equals(String.valueOf(sn))) {
                return map;
            }
        }
        return index < skuList.size() ? skuList.get(index) : skuList.get(0);
    }

    private static String stringVal(Object value) {
        return value == null ? null : String.valueOf(value);
    }

    private static Integer intVal(Object value) {
        if (value == null || CharSequenceUtil.isBlank(String.valueOf(value))) {
            return null;
        }
        return NumberUtil.parseInt(String.valueOf(value));
    }

    private static Boolean boolVal(Object value, boolean defaultVal) {
        if (value == null) {
            return defaultVal;
        }
        if (value instanceof Boolean b) {
            return b;
        }
        return Boolean.parseBoolean(String.valueOf(value));
    }

    private static Date dateVal(Object value) {
        if (value == null || CharSequenceUtil.isBlank(String.valueOf(value))) {
            return null;
        }
        return DateUtil.parse(String.valueOf(value));
    }
}
