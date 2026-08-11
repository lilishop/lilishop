package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotEmpty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 拼团查询通用类
 *
 * @author paulG
 * @since 2020/10/9
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class PintuanSearchParams extends BasePromotionsSearchParams {

    @Schema(description = "商家名称，如果是平台，这个值为 platform")
    private String storeName;

    @NotEmpty(message = "活动名称不能为空")
    @Schema(description = "活动名称", requiredMode = Schema.RequiredMode.REQUIRED)
    private String promotionName;


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
