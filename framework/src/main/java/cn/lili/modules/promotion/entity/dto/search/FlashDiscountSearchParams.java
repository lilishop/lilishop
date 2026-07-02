package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

@Data
@EqualsAndHashCode(callSuper = true)
public class FlashDiscountSearchParams extends BasePromotionsSearchParams {

    @Schema(description = "活动名称")
    private String promotionName;

    @Schema(description = "店铺名称")
    private String storeName;

    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("promotion_name", promotionName);
        }
        if (CharSequenceUtil.isNotEmpty(storeName)) {
            queryWrapper.like("store_name", storeName);
        }
        return queryWrapper;
    }
}
