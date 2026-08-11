package cn.lili.modules.distribution.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.distribution.entity.enums.DistributionGoodsDefaultSortEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 分销商品设置读取辅助
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionGoodsSettingSupport {

    @Autowired
    private SettingService settingService;

    /**
     * 读取主推商品设置。
     */
    public DistributionFeaturedGoodsSetting readSetting() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_FEATURED_GOODS_SETTING.name());
        if (setting != null && setting.getSettingValue() != null) {
            DistributionFeaturedGoodsSetting value = JSONUtil.toBean(setting.getSettingValue(), DistributionFeaturedGoodsSetting.class);
            if (value != null) {
                return value;
            }
        }
        return new DistributionFeaturedGoodsSetting();
    }

    /**
     * 将默认排序枚举映射为查询字段名。
     *
     * @param defaultSort 默认排序枚举值
     * @return 排序字段
     */
    public String mapDefaultSortToField(String defaultSort) {
        if (DistributionGoodsDefaultSortEnum.HIGHEST_PRICE.name().equals(defaultSort)) {
            return "PRICE";
        }
        if (DistributionGoodsDefaultSortEnum.HIGHEST_SALES.name().equals(defaultSort)) {
            return "SALES";
        }
        if (DistributionGoodsDefaultSortEnum.LATEST_LISTING.name().equals(defaultSort)) {
            return "LATEST";
        }
        return "COMMISSION";
    }

    /**
     * 按后台配置的默认排序规则应用排序。
     *
     * @param queryWrapper 查询构造器
     */
    public void applyMemberDefaultSort(QueryWrapper<?> queryWrapper) {
        applyMemberSort(queryWrapper, null, null);
    }

    /**
     * 按指定或默认规则应用分销商品排序。
     *
     * @param queryWrapper 查询构造器
     * @param sortField    排序字段
     * @param sortOrder    排序方向
     */
    public void applyMemberSort(QueryWrapper<?> queryWrapper, String sortField, String sortOrder) {
        if (queryWrapper == null) {
            return;
        }
        if (CharSequenceUtil.isBlank(sortField)) {
            // 未指定排序时读取后台默认规则，缺省按最高佣金降序
            String defaultSort = readSetting().getDefaultSort();
            if (CharSequenceUtil.isBlank(defaultSort)) {
                defaultSort = DistributionGoodsDefaultSortEnum.HIGHEST_COMMISSION.name();
            }
            sortField = mapDefaultSortToField(defaultSort);
            sortOrder = "DESC";
        }
        boolean asc = "ASC".equalsIgnoreCase(sortOrder);
        if ("PRICE".equals(sortField)) {
            queryWrapper.orderBy(true, asc, "dg.price");
            return;
        }
        if ("SALES".equals(sortField)) {
            queryWrapper.orderBy(true, asc, "IFNULL(gs.buy_count, 0)");
            // 销量相同时按佣金二次排序
            queryWrapper.orderByDesc("dg.commission");
            return;
        }
        if ("LATEST".equals(sortField)) {
            queryWrapper.orderBy(true, asc, "dg.create_time");
            return;
        }
        queryWrapper.orderBy(true, asc, "dg.commission");
    }
}
