package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Date;
import java.util.regex.Pattern;

/**
 * 满优惠查询通用类
 *
 * @author paulG
 * @date 2020/8/21
 **/
@Data
public class FullDiscountSearchParams implements Serializable {

    private static final long serialVersionUID = -4052716630253333681L;


    @ApiModelProperty(value = "活动名称")
    private String promotionName;

    @ApiModelProperty(value = "店铺编号 如有多个','分割")
    private String storeId;

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
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("title", promotionName);
        }
        if (storeId != null) {
            queryWrapper.in("store_id", Arrays.asList(storeId.split(",")));
        }
        if (startTime != null) {
            queryWrapper.ge("start_time", new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.le("end_time", new Date(endTime));
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            queryWrapper.eq("promotion_status", PromotionStatusEnum.valueOf(promotionStatus).name());
        }
        return queryWrapper;
    }

    public Query mongoQuery() {
        Query query = new Query();
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            Pattern pattern = Pattern.compile("^.*" + promotionName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("promotionName").regex(pattern));
        }
        if (storeId != null) {
            query.addCriteria(Criteria.where("storeId").in(Arrays.asList(storeId.split(","))));
        }
        if (startTime != null) {
            query.addCriteria(Criteria.where("startTime").gte(new Date(startTime)));
        }
        if (endTime != null) {
            query.addCriteria(Criteria.where("endTime").lte(new Date(endTime)));
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            query.addCriteria(Criteria.where("promotionStatus").is(PromotionStatusEnum.valueOf(promotionStatus).name()));
        }
        query.addCriteria(Criteria.where("deleteFlag").is(false));
        return query;
    }

}
