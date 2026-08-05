package cn.lili.modules.system.entity.dto;

import cn.lili.modules.distribution.entity.enums.DistributionFeaturedMatchRuleEnum;
import cn.lili.modules.distribution.entity.enums.DistributionFeaturedSourceModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionGoodsDefaultSortEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 分销主推商品设置
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionFeaturedGoodsSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * @see DistributionGoodsDefaultSortEnum
     */
    @Schema(description = "商品默认排序")
    private String defaultSort = DistributionGoodsDefaultSortEnum.HIGHEST_COMMISSION.name();

    @Schema(description = "商品默认分享文案")
    private String defaultShareText;

    @Schema(description = "分销员个人中心是否展示主推商品")
    private Boolean centerVisible = Boolean.FALSE;

    @Schema(description = "是否展示主推商品销量")
    private Boolean showSales = Boolean.FALSE;

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionFeaturedSourceModeEnum
     */
    @Schema(description = "主推商品来源：AUTO自动匹配 CUSTOM自定义")
    private String sourceMode = DistributionFeaturedSourceModeEnum.AUTO.name();

    /**
     * @see cn.lili.modules.distribution.entity.enums.DistributionFeaturedMatchRuleEnum
     */
    @Schema(description = "自动匹配规则：HIGHEST_COMMISSION最高佣金 HIGHEST_SALES最高销量")
    private String matchRule = DistributionFeaturedMatchRuleEnum.HIGHEST_COMMISSION.name();

    @Schema(description = "主推商品数量：5或10")
    private Integer displayCount = 5;

    @Schema(description = "自定义主推商品规格ID列表（按展示顺序）")
    private List<String> skuIds = new ArrayList<>();
}
