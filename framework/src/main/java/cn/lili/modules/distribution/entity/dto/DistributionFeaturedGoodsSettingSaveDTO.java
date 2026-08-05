package cn.lili.modules.distribution.entity.dto;

import cn.lili.modules.system.entity.dto.DistributionFeaturedGoodsSetting;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 保存分销商品设置
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "保存分销商品设置")
public class DistributionFeaturedGoodsSettingSaveDTO extends DistributionFeaturedGoodsSetting {
}
