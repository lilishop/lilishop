package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dto.DistributionFeaturedGoodsSettingSaveDTO;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedMatchRuleEnum;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedSourceModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionGoodsAuthEnum;
import cn.lili.modules.distribution.entity.enums.DistributionGoodsDefaultSortEnum;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsItemVO;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsSettingVO;
import cn.lili.modules.distribution.mapper.DistributionFeaturedGoodsMapper;
import cn.lili.modules.distribution.service.DistributionFeaturedGoodsService;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 分销主推商品服务实现
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Service
public class DistributionFeaturedGoodsServiceImpl implements DistributionFeaturedGoodsService {

    @Autowired
    private SettingService settingService;

    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Autowired
    private DistributionFeaturedGoodsMapper distributionFeaturedGoodsMapper;

    /**
     * 获取主推商品设置详情。
     */
    @Override
    public DistributionFeaturedGoodsSettingVO getSettingDetail() {
        DistributionFeaturedGoodsSetting setting = readSetting();
        DistributionFeaturedGoodsSettingVO vo = new DistributionFeaturedGoodsSettingVO();
        BeanUtil.copyProperties(setting, vo);
        vo.setCustomGoods(listAllCustomGoods(setting));
        return vo;
    }

    /**
     * 保存主推商品设置。
     *
     * @param dto 设置参数
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void saveSetting(DistributionFeaturedGoodsSettingSaveDTO dto) {
        normalizeSetting(dto);
        validateSetting(dto);
        // 自定义模式：校验并保留已审核通过的 SKU；自动模式：清空 SKU 列表
        if (DistributionFeaturedSourceModeEnum.CUSTOM.name().equals(dto.getSourceMode())) {
            dto.setSkuIds(resolveValidSkuIds(dto.getSkuIds()));
        } else {
            dto.setSkuIds(new ArrayList<>());
        }
        saveSettingJson(dto);
    }

    /**
     * 买家端获取分销中心展示的主推商品列表。
     *
     * @return 主推商品列表
     */
    @Override
    public List<DistributionFeaturedGoodsItemVO> listFeaturedForBuyer() {
        DistributionFeaturedGoodsSetting setting = readSetting();
        // 分销中心未开启展示时直接返回空列表
        if (!Boolean.TRUE.equals(setting.getCenterVisible())) {
            return Collections.emptyList();
        }
        int limit = normalizeDisplayCount(setting.getDisplayCount());
        // 按来源模式（自定义/自动）解析商品，再按配置控制销量字段可见性
        List<DistributionFeaturedGoodsItemVO> list = resolveFeaturedGoods(setting, limit);
        applySalesVisibility(setting, list);
        return list;
    }

    /**
     * 管理端回显全部可解析的自定义主推商品（不截断）。
     *
     * @param setting 主推商品设置
     * @return 自定义主推商品列表
     */
    private List<DistributionFeaturedGoodsItemVO> listAllCustomGoods(DistributionFeaturedGoodsSetting setting) {
        return listCustomGoods(setting, null);
    }

    /**
     * 按配置中的 SKU 列表查询自定义主推商品，并保持配置顺序。
     *
     * @param setting 主推商品设置
     * @param limit   最大返回条数，null 表示不截断
     * @return 自定义主推商品列表
     */
    private List<DistributionFeaturedGoodsItemVO> listCustomGoods(DistributionFeaturedGoodsSetting setting, Integer limit) {
        if (CollUtil.isEmpty(setting.getSkuIds())) {
            return Collections.emptyList();
        }
        List<String> skuIds = setting.getSkuIds().stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(id -> !id.isEmpty())
                .collect(Collectors.toList());
        if (limit != null) {
            skuIds = skuIds.stream().limit(limit).collect(Collectors.toList());
        }
        if (CollUtil.isEmpty(skuIds)) {
            return Collections.emptyList();
        }
        return orderBySkuIds(distributionFeaturedGoodsMapper.listBySkuIds(skuIds), skuIds);
    }

    /**
     * 根据来源模式（自定义/自动）解析买家端展示的主推商品。
     *
     * @param setting 主推商品设置
     * @param limit   展示条数上限
     * @return 主推商品列表
     */
    private List<DistributionFeaturedGoodsItemVO> resolveFeaturedGoods(DistributionFeaturedGoodsSetting setting, int limit) {
        if (DistributionFeaturedSourceModeEnum.CUSTOM.name().equals(setting.getSourceMode())) {
            return listCustomGoods(setting, limit);
        }
        // 自动模式：按销量或佣金排序取 Top N
        if (DistributionFeaturedMatchRuleEnum.HIGHEST_SALES.name().equals(setting.getMatchRule())) {
            return distributionFeaturedGoodsMapper.listAutoBySales(limit);
        }
        return distributionFeaturedGoodsMapper.listAutoByCommission(limit);
    }

    /**
     * 按配置的 SKU 顺序重排查询结果。
     *
     * @param items  查询到的商品列表
     * @param skuIds 目标 SKU 顺序
     * @return 排序后的商品列表
     */
    private List<DistributionFeaturedGoodsItemVO> orderBySkuIds(List<DistributionFeaturedGoodsItemVO> items, List<String> skuIds) {
        if (CollUtil.isEmpty(items) || CollUtil.isEmpty(skuIds)) {
            return Collections.emptyList();
        }
        // 先建 SKU -> 商品映射，再按配置中的 SKU 顺序重排
        Map<String, DistributionFeaturedGoodsItemVO> itemMap = items.stream()
                .filter(item -> item.getSkuId() != null)
                .collect(Collectors.toMap(DistributionFeaturedGoodsItemVO::getSkuId, Function.identity(), (a, b) -> a, LinkedHashMap::new));
        List<DistributionFeaturedGoodsItemVO> ordered = new ArrayList<>();
        for (String skuId : skuIds) {
            DistributionFeaturedGoodsItemVO item = itemMap.get(skuId);
            if (item != null) {
                ordered.add(item);
            }
        }
        return ordered;
    }

    /**
     * 校验并过滤出已审核通过的分销商品 SKU。
     *
     * @param skuIds 待校验的 SKU 列表
     * @return 有效的 SKU 列表
     */
    private List<String> resolveValidSkuIds(List<String> skuIds) {
        List<String> distinctSkuIds = normalizeSkuIds(skuIds);
        if (CollUtil.isEmpty(distinctSkuIds)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请至少选择一个自定义主推商品");
        }
        List<DistributionGoods> goodsList = distributionGoodsService.list(new LambdaQueryWrapper<DistributionGoods>()
                .in(DistributionGoods::getSkuId, distinctSkuIds)
                .eq(DistributionGoods::getAuthStatus, DistributionGoodsAuthEnum.PASS.name()));
        if (CollUtil.isEmpty(goodsList)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择已审核通过的分销商品");
        }
        Map<String, DistributionGoods> goodsBySkuId = goodsList.stream()
                .filter(goods -> goods.getSkuId() != null)
                .collect(Collectors.toMap(DistributionGoods::getSkuId, Function.identity(), (a, b) -> a, LinkedHashMap::new));
        List<String> validSkuIds = new ArrayList<>();
        for (String skuId : distinctSkuIds) {
            if (goodsBySkuId.containsKey(skuId)) {
                validSkuIds.add(skuId);
            }
        }
        if (CollUtil.isEmpty(validSkuIds)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请选择已审核通过的分销商品");
        }
        return validSkuIds;
    }

    /**
     * 去重并清洗 SKU ID 列表（去空白、去空串）。
     *
     * @param skuIds 原始 SKU 列表
     * @return 规范化后的 SKU 列表
     */
    private List<String> normalizeSkuIds(List<String> skuIds) {
        if (CollUtil.isEmpty(skuIds)) {
            return Collections.emptyList();
        }
        return skuIds.stream()
                .filter(Objects::nonNull)
                .map(String::trim)
                .filter(id -> !id.isEmpty())
                .distinct()
                .collect(Collectors.toList());
    }

    /**
     * 读取主推商品设置（优先新 key，兼容旧版 DISTRIBUTION_SETTING 内嵌配置）。
     *
     * @return 主推商品设置
     */
    private DistributionFeaturedGoodsSetting readSetting() {
        // 优先读取独立配置项，兼容旧版 DISTRIBUTION_SETTING 内嵌节点
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_FEATURED_GOODS_SETTING.name());
        if (setting != null && setting.getSettingValue() != null) {
            DistributionFeaturedGoodsSetting value = JSONUtil.toBean(setting.getSettingValue(), DistributionFeaturedGoodsSetting.class);
            normalizeSetting(value);
            return value;
        }
        DistributionFeaturedGoodsSetting legacy = readLegacySettingFromDistributionSetting();
        if (legacy != null) {
            normalizeSetting(legacy);
            return legacy;
        }
        return defaultSetting();
    }

    /**
     * 从旧版 {@link SettingEnum#DISTRIBUTION_SETTING} 中读取 featuredGoods 节点。
     *
     * @return 旧版主推商品设置，不存在时返回 null
     */
    private DistributionFeaturedGoodsSetting readLegacySettingFromDistributionSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTING.name());
        if (setting == null || setting.getSettingValue() == null) {
            return null;
        }
        cn.hutool.json.JSONObject json = JSONUtil.parseObj(setting.getSettingValue());
        if (!json.containsKey("featuredGoods") || json.get("featuredGoods") == null) {
            return null;
        }
        return json.getBean("featuredGoods", DistributionFeaturedGoodsSetting.class);
    }

    /**
     * 将主推商品设置序列化写入系统配置表。
     *
     * @param setting 主推商品设置
     */
    private void saveSettingJson(DistributionFeaturedGoodsSetting setting) {
        Setting entity = settingService.getById(SettingEnum.DISTRIBUTION_FEATURED_GOODS_SETTING.name());
        if (entity == null) {
            entity = new Setting();
            entity.setId(SettingEnum.DISTRIBUTION_FEATURED_GOODS_SETTING.name());
        }
        entity.setSettingValue(JSONUtil.toJsonStr(setting));
        settingService.saveUpdate(entity);
    }

    /**
     * 按配置控制买家端是否展示销量字段。
     *
     * @param setting 主推商品设置
     * @param list    商品列表
     */
    private void applySalesVisibility(DistributionFeaturedGoodsSetting setting, List<DistributionFeaturedGoodsItemVO> list) {
        if (CollUtil.isEmpty(list)) {
            return;
        }
        // 未开启销量展示时统一置空；开启后零销量也不展示
        if (!Boolean.TRUE.equals(setting.getShowSales())) {
            list.forEach(item -> item.setSalesCount(null));
            return;
        }
        list.forEach(item -> {
            if (item.getSalesCount() == null || item.getSalesCount() <= 0) {
                item.setSalesCount(null);
            }
        });
    }

    /**
     * 构建空的主推商品默认设置。
     *
     * @return 默认设置对象
     */
    private DistributionFeaturedGoodsSetting defaultSetting() {
        return new DistributionFeaturedGoodsSetting();
    }

    /**
     * 补全主推商品设置中的可空字段与默认值。
     *
     * @param setting 主推商品设置
     */
    private void normalizeSetting(DistributionFeaturedGoodsSetting setting) {
        if (CharSequenceUtil.isBlank(setting.getDefaultSort())) {
            setting.setDefaultSort(DistributionGoodsDefaultSortEnum.HIGHEST_COMMISSION.name());
        }
        if (setting.getDefaultShareText() != null) {
            setting.setDefaultShareText(setting.getDefaultShareText().trim());
        }
        if (setting.getCenterVisible() == null) {
            setting.setCenterVisible(Boolean.FALSE);
        }
        if (setting.getShowSales() == null) {
            setting.setShowSales(Boolean.FALSE);
        }
        if (setting.getSourceMode() == null) {
            setting.setSourceMode(DistributionFeaturedSourceModeEnum.AUTO.name());
        }
        if (setting.getMatchRule() == null) {
            setting.setMatchRule(DistributionFeaturedMatchRuleEnum.HIGHEST_COMMISSION.name());
        }
        setting.setDisplayCount(normalizeDisplayCount(setting.getDisplayCount()));
        if (setting.getSkuIds() == null) {
            setting.setSkuIds(new ArrayList<>());
        }
    }

    /**
     * 规范化展示条数（仅支持 5 或 10，默认 5）。
     *
     * @param displayCount 原始展示条数
     * @return 规范化后的条数
     */
    private int normalizeDisplayCount(Integer displayCount) {
        if (displayCount != null && displayCount == 10) {
            return 10;
        }
        return 5;
    }

    /**
     * 校验保存时的主推商品设置参数。
     *
     * @param dto 待保存的设置
     */
    private void validateSetting(DistributionFeaturedGoodsSettingSaveDTO dto) {
        if (!isValidDefaultSort(dto.getDefaultSort())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "商品默认排序设置不正确");
        }
        if (!DistributionFeaturedSourceModeEnum.AUTO.name().equals(dto.getSourceMode())
                && !DistributionFeaturedSourceModeEnum.CUSTOM.name().equals(dto.getSourceMode())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "主推商品来源设置不正确");
        }
        // 自动模式校验匹配规则；自定义模式校验 SKU 列表
        if (DistributionFeaturedSourceModeEnum.AUTO.name().equals(dto.getSourceMode())) {
            if (!DistributionFeaturedMatchRuleEnum.HIGHEST_COMMISSION.name().equals(dto.getMatchRule())
                    && !DistributionFeaturedMatchRuleEnum.HIGHEST_SALES.name().equals(dto.getMatchRule())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "主推商品匹配规则设置不正确");
            }
            return;
        }
        if (CollUtil.isEmpty(dto.getSkuIds())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "请至少选择一个自定义主推商品");
        }
        int displayCount = normalizeDisplayCount(dto.getDisplayCount());
        int skuCount = normalizeSkuIds(dto.getSkuIds()).size();
        if (skuCount > displayCount) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, customGoodsLimitMessage(displayCount));
        }
    }

    /**
     * 自定义主推商品超过主推条数时的提示文案。
     *
     * @param displayCount 主推条数
     * @return 提示文案
     */
    private String customGoodsLimitMessage(int displayCount) {
        return "主推商品数量为" + displayCount + "时，最多选择" + displayCount + "个自定义商品";
    }

    /**
     * 判断商品默认排序枚举值是否合法。
     *
     * @param defaultSort 排序方式
     * @return 是否合法（空值视为合法）
     */
    private boolean isValidDefaultSort(String defaultSort) {
        if (CharSequenceUtil.isBlank(defaultSort)) {
            return true;
        }
        for (DistributionGoodsDefaultSortEnum item : DistributionGoodsDefaultSortEnum.values()) {
            if (item.name().equals(defaultSort)) {
                return true;
            }
        }
        return false;
    }
}
