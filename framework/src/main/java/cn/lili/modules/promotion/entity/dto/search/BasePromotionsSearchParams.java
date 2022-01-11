package cn.lili.modules.promotion.entity.dto.search;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.PromotionsScopeTypeEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Arrays;
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
    @ApiModelProperty(value = "活动状态 如需同时判断多个活动状态','分割")
    private String promotionStatus;

    /**
     * @see PromotionsScopeTypeEnum
     */
    @ApiModelProperty(value = "关联范围类型")
    private String scopeType;

    @ApiModelProperty(value = "店铺编号 如有多个','分割")
    private String storeId;

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
            queryWrapper.and(i -> {
                for (String status : promotionStatus.split(",")) {
                    i.or(PromotionTools.queryPromotionStatus(PromotionsStatusEnum.valueOf(status)));
                }
            });
        }
        if (CharSequenceUtil.isNotEmpty(scopeType)) {
            queryWrapper.eq("scope_type", scopeType);
        }
        if (CharSequenceUtil.isNotEmpty(storeId)) {
            queryWrapper.in("store_id", Arrays.asList(storeId.split(",")));
        }
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }


}
