package cn.lili.modules.distribution.entity.vos;

import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.ArrayList;
import java.util.List;

/**
 * 分销商品设置详情
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class DistributionFeaturedGoodsSettingVO extends DistributionFeaturedGoodsSetting {

    @Schema(description = "自定义主推商品列表")
    private List<DistributionFeaturedGoodsItemVO> customGoods = new ArrayList<>();
}
