package cn.lili.modules.promotion.entity.vos;


import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.*;
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
public class KanJiaActivityGoodsParams implements Serializable {

    private static final long serialVersionUID = 1344104067705714289L;

    @ApiModelProperty(value = "活动名称")
    private String goodsName;


    @ApiModelProperty(value = "活动开始时间", required = true)
    private Long startTime;

    @ApiModelProperty(value = "活动结束时间", required = true)
    private Long endTime;

    /**
     * @see PromotionStatusEnum
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
            queryWrapper.ge("start_time", new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.le("end_time", new Date(endTime));
        }
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }



}
