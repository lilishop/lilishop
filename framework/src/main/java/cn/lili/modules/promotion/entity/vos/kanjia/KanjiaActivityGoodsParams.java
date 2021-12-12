package cn.lili.modules.promotion.entity.vos.kanjia;


import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;

/**
 * 砍价活动商品查询通用类
 *
 * @author qiuqiu
 * @date 2020/8/21
 **/
@Data
public class KanjiaActivityGoodsParams implements Serializable {

    private static final long serialVersionUID = 1344104067705714289L;

    @ApiModelProperty(value = "活动商品")
    private String goodsName;

    @ApiModelProperty(value = "活动开始时间")
    private Long startTime;

    @ApiModelProperty(value = "活动结束时间")
    private Long endTime;

    @ApiModelProperty(value = "skuId")
    private String skuId;

    /**
     * @see PromotionsStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    private String promotionStatus;

    public <T> QueryWrapper<T> wrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();

        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (promotionStatus != null) {
            queryWrapper.eq("promotion_status", promotionStatus);
        }
        if (startTime != null) {
            queryWrapper.le("start_time", new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.ge("end_time", new Date(endTime));
        }
        if (UserContext.getCurrentUser() != null && UserContext.getCurrentUser().getRole().equals(UserEnums.MEMBER)) {

            queryWrapper.gt("stock", 0);
        }
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }

}
