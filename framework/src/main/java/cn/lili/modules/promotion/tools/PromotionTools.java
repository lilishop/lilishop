package cn.lili.modules.promotion.tools;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.extern.slf4j.Slf4j;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;


/**
 * 优惠活动通用验证类
 *
 * @author paulG
 * @since 2020/8/18
 **/
@Slf4j
public class PromotionTools {

    public static final String START_TIME_COLUMN = "start_time";
    public static final String END_TIME_COLUMN = "end_time";
    public static final String PLATFORM_ID = "0";
    public static final String PLATFORM_NAME = "platform";

    /**
     * 参数验证
     * 1、活动起始时间必须大于当前时间
     * 2、验证活动开始时间是否大于活动结束时间
     *
     * @param startTime 活动开始时间
     * @param endTime   活动结束时间
     */
    public static void checkPromotionTime(Date startTime, Date endTime) {

        if (startTime == null) {
            throw new ServiceException(ResultCode.PROMOTION_TIME_NOT_EXIST);
        }

        DateTime now = DateUtil.date();

        //如果活动起始时间小于现在时间
        if (now.after(startTime)) {
            throw new ServiceException(ResultCode.PROMOTION_START_TIME_ERROR);
        }
        //如果活动结束时间小于现在时间
        if (endTime != null && now.after(endTime)) {
            throw new ServiceException(ResultCode.PROMOTION_END_TIME_ERROR);
        }

        //开始时间不能大于结束时间
        if (endTime != null && startTime.after(endTime)) {
            throw new ServiceException(ResultCode.PROMOTION_TIME_ERROR);
        }
    }

    /**
     * 组装检查促销活动时间 query wrapper
     *
     * @param startTime  开始时间
     * @param endTime    结束时间
     * @param typeEnum   促销类型
     * @param storeId    店铺id
     * @param activityId 排除的促销活动id
     * @return mybatis plus query wrapper对象
     */
    public static <T extends BasePromotions> QueryWrapper<T> checkActiveTime(Date startTime, Date endTime, PromotionTypeEnum typeEnum, String storeId, String activityId) {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (PromotionTypeEnum.SECKILL != typeEnum) {
            queryWrapper.nested(i -> {
                //新活动起始时间 大于 之前活动的起始时间 小于 之前活动的截止时间
                i.nested(i2 -> i2.le(START_TIME_COLUMN, startTime).ge(END_TIME_COLUMN, startTime));
                //新活动结束时间 大于 之前活动的起始时间 小于 之前活动的截止时间
                i.or(i1 -> i1.le(START_TIME_COLUMN, endTime).ge(END_TIME_COLUMN, endTime));
            });
        } else {
            queryWrapper.ge(START_TIME_COLUMN, DateUtil.beginOfDay(startTime)).le(END_TIME_COLUMN, DateUtil.endOfDay(endTime));
        }
        if (storeId != null) {
            queryWrapper.eq("store_id", storeId);
        }
        if (activityId != null) {
            queryWrapper.ne("id", activityId);
        }
        queryWrapper.and(i -> i.or(queryPromotionStatus(PromotionsStatusEnum.NEW)).or(queryPromotionStatus(PromotionsStatusEnum.START)));
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }


    public static <T> Consumer<QueryWrapper<T>> queryPromotionStatus(PromotionsStatusEnum promotionsStatusEnum) {
        switch (promotionsStatusEnum) {
            case NEW:
                return (QueryWrapper<T> t) -> t.nested(i -> i.gt(START_TIME_COLUMN, new Date()).gt(END_TIME_COLUMN, new Date()));
            case START:
                return (QueryWrapper<T> t) -> t.nested(i -> i.le(START_TIME_COLUMN, new Date()).ge(END_TIME_COLUMN, new Date()));
            case END:
                return (QueryWrapper<T> t) -> t.nested(i -> i.lt(START_TIME_COLUMN, new Date()).lt(END_TIME_COLUMN, new Date()));
            case CLOSE:
                return (QueryWrapper<T> t) -> t.nested(i -> i.isNull(START_TIME_COLUMN).isNull(END_TIME_COLUMN));
            default:
                return null;
        }
    }

    /**
     * 促销商品入库前填充
     *
     * @param originList 原促销商品列表
     * @param promotion  促销信息
     * @return 促销商品列表
     */
    public static List<PromotionGoods> promotionGoodsInit(List<PromotionGoods> originList, BasePromotions promotion, PromotionTypeEnum promotionTypeEnum) {
        if (originList != null) {
            //本次促销商品入库
            for (PromotionGoods promotionGoods : originList) {
                promotionGoods.setPromotionId(promotion.getId());
                if (CharSequenceUtil.isEmpty(promotionGoods.getStoreId())) {
                    promotionGoods.setStoreId(promotion.getStoreId());
                }
                if (CharSequenceUtil.isEmpty(promotionGoods.getStoreName())) {
                    promotionGoods.setStoreName(promotion.getStoreName());
                }
                promotionGoods.setTitle(promotion.getPromotionName());
                // 如果是秒杀活动保留原时间
                if (promotionGoods.getStartTime() == null || !PromotionTypeEnum.SECKILL.equals(promotionTypeEnum)) {
                    promotionGoods.setStartTime(promotion.getStartTime());
                }
                if (promotionGoods.getStartTime() == null || !PromotionTypeEnum.SECKILL.equals(promotionTypeEnum)) {
                    promotionGoods.setEndTime(promotion.getEndTime());
                }
                promotionGoods.setStartTime(promotion.getStartTime());
                promotionGoods.setEndTime(promotion.getEndTime());
                promotionGoods.setPromotionType(promotionTypeEnum.name());
                promotionGoods.setNum(0);
                promotionGoods.setDeleteFlag(promotion.getDeleteFlag());
            }
        }
        return originList;
    }


    public static int nextHour(String[] totalHours, Integer timeline) {
        int nextHour = 23;
        int[] hoursSored = Arrays.stream(totalHours).mapToInt(Integer::parseInt).toArray();
        Arrays.sort(hoursSored);
        for (int i : hoursSored) {
            if (timeline < i) {
                nextHour = i;
                break;
            }
        }
        return nextHour;
    }

    public static Map<String, Object> filterInvalidPromotionsMap(Map<String, Object> map) {
        if (CollUtil.isEmpty(map)) {
            return new HashMap<>();
        }
        try {
            //移除无效促销活动
            return map.entrySet().stream().filter(Objects::nonNull).filter(i -> {
                JSONObject promotionsObj = JSONUtil.parseObj(i.getValue());
                BasePromotions basePromotions = promotionsObj.toBean(BasePromotions.class);
                if (basePromotions != null && basePromotions.getStartTime() != null && basePromotions.getEndTime() != null) {
                    return basePromotions.getStartTime().getTime() <= System.currentTimeMillis() && basePromotions.getEndTime().getTime() >= System.currentTimeMillis();
                }
                return i.getValue() != null;
            }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue, (oldValue, newValue) -> newValue));
        } catch (Exception e) {
            log.error("过滤无效促销活动出现异常。异常促销信息：{}，异常信息：{} ", map, e);
            return new HashMap<>();
        }
    }

}
