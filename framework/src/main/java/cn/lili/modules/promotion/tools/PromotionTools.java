package cn.lili.modules.promotion.tools;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.query.Query;

import java.util.Date;
import java.util.List;


/**
 * 优惠活动通用验证类
 *
 * @author paulG
 * @date 2020/8/18
 **/
public class PromotionTools {

    /**
     * 参数验证
     * 1、活动起始时间必须大于当前时间
     * 2、验证活动开始时间是否大于活动结束时间
     *
     * @param startTime 活动开始时间
     * @param endTime   活动结束时间
     * @param num       参与活动商品数量
     * @param goodsList 选择的商品
     */
    public static void paramValid(Long startTime, Long endTime, int num, List<PromotionGoods> goodsList) {

        checkPromotionTime(startTime, endTime);

        //如果促销活动选择的是部分商品参加活动
        if (num != -1 && goodsList == null) {
            throw new ServiceException(ResultCode.PROMOTION_GOODS_ERROR);
        }
    }

    /**
     * 参数验证
     * 1、活动起始时间必须大于当前时间
     * 2、验证活动开始时间是否大于活动结束时间
     *
     * @param startTime 活动开始时间
     * @param endTime   活动结束时间
     */
    public static void checkPromotionTime(Long startTime, Long endTime) {

        long nowTime = DateUtil.getDateline() * 1000;

        //如果活动起始时间小于现在时间
        if (startTime < nowTime) {
            throw new ServiceException(ResultCode.PROMOTION_START_TIME_ERROR);
        }

        //开始时间不能大于结束时间
        if (startTime > endTime) {
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
     * @param activityId 促销活动id
     * @param <T>        促销类型
     * @return mybatis plus query wrapper对象
     */
    public static <T extends BasePromotion> QueryWrapper<T> checkActiveTime(Date startTime, Date endTime, PromotionTypeEnum typeEnum, String storeId, String activityId) {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        String startTimeColumn = "start_time";
        String endTimeColumn = "end_time";
        if (PromotionTypeEnum.SECKILL != typeEnum) {
            queryWrapper.nested(i -> {
                //新活动起始时间 大于 之前活动的起始时间 小于 之前活动的截止时间
                i.nested(i2 -> i2.le(startTimeColumn, startTime).ge(endTimeColumn, startTime));
                //新活动结束时间 大于 之前活动的起始时间 小于 之前活动的截止时间
                i.or(i1 -> i1.le(startTimeColumn, endTime).ge(endTimeColumn, endTime));
            });
        } else {
            queryWrapper.ge(startTimeColumn, cn.hutool.core.date.DateUtil.beginOfDay(startTime)).le(endTimeColumn, cn.hutool.core.date.DateUtil.endOfDay(endTime));
        }
        if (storeId != null) {
            queryWrapper.eq("store_id", storeId);
        }
        if (activityId != null) {
            queryWrapper.ne("id", activityId);
        }
        //忽略已作废和已关闭的活动
        queryWrapper.ne("promotion_status", PromotionStatusEnum.END.name());
        queryWrapper.ne("promotion_status", PromotionStatusEnum.CLOSE.name());
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }

    /**
     * 促销商品入库前填充
     *
     * @param originList 原促销商品列表
     * @param promotion  促销信息
     * @return 促销商品列表
     */
    public static List<PromotionGoods> promotionGoodsInit(List<PromotionGoods> originList, BasePromotion promotion, PromotionTypeEnum promotionTypeEnum) {
        //本次促销商品入库
        for (PromotionGoods promotionGoods : originList) {
            promotionGoods.setPromotionId(promotion.getId());
            promotionGoods.setStoreName(promotion.getStoreName());
            promotionGoods.setTitle(promotion.getPromotionName());
            promotionGoods.setStartTime(promotion.getStartTime());
            if (promotion.getEndTime() == null) {
                promotionGoods.setEndTime(promotion.getEndTime());
            }
            promotionGoods.setPromotionType(promotionTypeEnum.name());
            promotionGoods.setPromotionStatus(promotion.getPromotionStatus());
            promotionGoods.setNum(0);
        }
        return originList;
    }

    /**
     * 为mongoQuery组织分页排序参数
     *
     * @param query 查询条件
     * @param page  分页排序参数
     */
    public static void mongoQueryPageParam(Query query, PageVO page) {
        page.setNotConvert(true);
        query.with(PageRequest.of(page.getMongoPageNumber(), page.getPageSize()));
        if (!CharSequenceUtil.isEmpty(page.getOrder()) && !CharSequenceUtil.isEmpty(page.getSort())) {
            query.with(Sort.by(Sort.Direction.valueOf(page.getOrder().toUpperCase()), page.getSort()));
        }
    }

}
