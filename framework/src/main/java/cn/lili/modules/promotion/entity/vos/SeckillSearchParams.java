package cn.lili.modules.promotion.entity.vos;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.ArrayUtil;
import cn.lili.modules.promotion.entity.enums.PromotionApplyStatusEnum;
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
 * 秒杀活动查询通用类
 *
 * @author paulG
 * @date 2020/8/21
 **/
@Data
public class SeckillSearchParams implements Serializable {

    private static final long serialVersionUID = -4052716630253333681L;

    @ApiModelProperty(value = "秒杀活动活动编号")
    private String seckillId;

    @ApiModelProperty(value = "活动名称")
    private String promotionName;

    @ApiModelProperty(value = "时刻")
    private Integer timeLine;

    @ApiModelProperty(value = "商家id")
    private String[] storeIds;

    @ApiModelProperty(value = "商家编号")
    private String storeId;

    @ApiModelProperty(value = "商品名称")
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

    /**
     * @see cn.lili.modules.promotion.entity.enums.PromotionApplyStatusEnum
     */
    @ApiModelProperty(value = "APPLY(\"申请\"), PASS(\"通过\"), REFUSE(\"拒绝\")")
    private String promotionApplyStatus;

    public <T> QueryWrapper<T> wrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            queryWrapper.like("promotion_name", promotionName);
        }
        if (CharSequenceUtil.isNotEmpty(seckillId)) {
            queryWrapper.eq("seckill_id", seckillId);
        }
        if (storeIds != null) {
            queryWrapper.in("store_id", Arrays.asList(storeIds));
        }
        if (timeLine != null) {
            queryWrapper.eq("time_line", timeLine);
        }
        if (startTime != null) {
            queryWrapper.ge("start_time", new Date(startTime));
        }
        if (endTime != null) {
            queryWrapper.le("end_time", new Date(endTime));
        }
        if (CharSequenceUtil.isNotEmpty(promotionApplyStatus)) {
            queryWrapper.eq("promotion_apply_status", PromotionApplyStatusEnum.valueOf(promotionApplyStatus).name());
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            queryWrapper.eq("promotion_status", PromotionStatusEnum.valueOf(promotionStatus).name());
        }
        return queryWrapper;
    }

    public Query mongoQuery() {
        Query query = new Query();
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            Pattern pattern = Pattern.compile("^.*" + goodsName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("goodsName").regex(pattern));
        }
        if (CharSequenceUtil.isNotEmpty(promotionName)) {
            Pattern pattern = Pattern.compile("^.*" + promotionName + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("promotionName").regex(pattern));
        }
        if (CharSequenceUtil.isNotEmpty(seckillId)) {
            query.addCriteria(Criteria.where("_id").is(seckillId));
        }
        if (storeIds != null) {
            Pattern pattern = Pattern.compile("^.*" + ArrayUtil.join(storeIds, ",") + ".*$", Pattern.CASE_INSENSITIVE);
            query.addCriteria(Criteria.where("storeIds").regex(pattern));
        }
        if (timeLine != null) {
            query.addCriteria(Criteria.where("timeLine").is(timeLine));
        }
        if (startTime != null) {
            query.addCriteria(Criteria.where("startTime").gte(new Date(startTime)));
        }
        if (endTime != null) {
            query.addCriteria(Criteria.where("endTime").lte(new Date(endTime)));
        }
        if (CharSequenceUtil.isNotEmpty(promotionApplyStatus)) {
            query.addCriteria(Criteria.where("promotionApplyStatus").is(PromotionApplyStatusEnum.valueOf(promotionApplyStatus).name()));
        }
        if (CharSequenceUtil.isNotEmpty(promotionStatus)) {
            query.addCriteria(Criteria.where("promotionStatus").is(PromotionStatusEnum.valueOf(promotionStatus).name()));
        }
        query.addCriteria(Criteria.where("deleteFlag").is(false));
        return query;
    }

}
