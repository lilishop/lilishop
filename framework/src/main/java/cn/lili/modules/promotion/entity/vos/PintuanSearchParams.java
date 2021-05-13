package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Date;
import java.util.regex.Pattern;

/**
 * 拼团查询通用类
 *
 * @author paulG
 * @date 2020/10/9
 **/
@Data
public class PintuanSearchParams {

    @ApiModelProperty(value = "商家id")
    private String storeId;

    @ApiModelProperty(value = "商家名称，如果是平台，这个值为 platform")
    private String storeName;

    @NotEmpty(message = "活动名称不能为空")
    @ApiModelProperty(value = "活动名称", required = true)
    private String promotionName;

    /**
     * @see PromotionStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    @NotNull(message = "活动状态不能为空")
    private String promotionStatus;

    @ApiModelProperty(value = "活动开始时间")
    private Long startTime;

    @ApiModelProperty(value = "活动结束时间")
    private Long endTime;

    public <T> QueryWrapper<T> wrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("promotion_name", promotionName);
        }
        if (!StringUtils.isEmpty(storeName)) {
            queryWrapper.like("store_name", storeName);
        }
        if (!StringUtils.isEmpty(storeId)) {
            queryWrapper.eq("store_id", storeName);
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
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }

    public Query mongoQuery() {
        Query query = new Query();
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            Pattern pattern = Pattern.compile("^.*" + promotionName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("promotionName").regex(pattern));
        }
        if (!StringUtils.isEmpty(storeName)) {
            Pattern pattern = Pattern.compile("^.*" + storeName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("storeName").regex(pattern));
        }
        if (!StringUtils.isEmpty(storeId)) {
            query.addCriteria(Criteria.where("storeId").is(storeId));
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
