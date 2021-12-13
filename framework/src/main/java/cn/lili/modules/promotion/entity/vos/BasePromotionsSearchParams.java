package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * @author paulG
 * @since 2021/11/18
 **/
@Data
public class BasePromotionsSearchParams {

    @ApiModelProperty(value = "活动id")
    private String id;

    @ApiModelProperty(value = "活动开始时间")
    private Long startTime;

    @ApiModelProperty(value = "活动结束时间")
    private Long endTime;

    /**
     * @see PromotionsStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    private String promotionStatus;

    /**
     * @see PromotionsScopeTypeEnum
     */
    @ApiModelProperty(value = "关联范围类型")
    private String scopeType;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();

        if (CharSequenceUtil.isNotEmpty(id)) {
            queryWrapper.eq("id", id);
        }
        if (startTime != null) {
            queryWrapper.ge("start_time", new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.le("end_time", new Date(endTime));
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            queryWrapper.and(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.valueOf(promotionStatus)));
        }
        if (CharSequenceUtil.isNotEmpty(scopeType)) {
            queryWrapper.eq("scope_type", scopeType);
        }
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }


}
