package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.enums.FullDiscountTypeEnum;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.FullDiscountService;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.PintuanService;
import cn.lili.modules.promotion.service.PointsGoodsService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.MarketingOverviewVO;
import cn.lili.modules.statistics.service.MarketingStatisticsService;
import cn.lili.modules.statistics.service.OrderStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * 营销概况统计业务层实现
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Service
public class MarketingStatisticsServiceImpl implements MarketingStatisticsService {

    @Autowired
    private CouponService couponService;
    @Autowired
    private SeckillService seckillService;
    @Autowired
    private PintuanService pintuanService;
    @Autowired
    private FullDiscountService fullDiscountService;
    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;
    @Autowired
    private PointsGoodsService pointsGoodsService;
    @Autowired
    private OrderStatisticsService orderStatisticsService;

    @Override
    public MarketingOverviewVO overview(StatisticsQueryParam statisticsQueryParam) {
        Date now = new Date();
        MarketingOverviewVO vo = new MarketingOverviewVO();
        vo.setCouponActiveNum(countActive(couponService, now));
        vo.setSeckillActiveNum(countActive(seckillService, now));
        vo.setPintuanActiveNum(countActive(pintuanService, now));
        vo.setFullDiscountActiveNum(countActive(fullDiscountService, now));
        vo.setFullMinusActiveNum(countActiveFullDiscountByType(now, FullDiscountTypeEnum.FULL_MINUS));
        vo.setFullRateActiveNum(countActiveFullDiscountByType(now, FullDiscountTypeEnum.FULL_RATE));
        vo.setFullGiftActiveNum(countActiveFullDiscountByType(now, FullDiscountTypeEnum.FULL_GIFT));
        vo.setKanjiaActiveNum(countActive(kanjiaActivityGoodsService, now));
        vo.setPointsGoodsActiveNum(countActive(pointsGoodsService, now));

        List<Coupon> coupons = couponService.list();
        long publish = 0L, received = 0L, used = 0L;
        if (coupons != null) {
            for (Coupon c : coupons) {
                publish += c.getPublishNum() == null ? 0 : c.getPublishNum();
                received += c.getReceivedNum() == null ? 0 : c.getReceivedNum();
                used += c.getUsedNum() == null ? 0 : c.getUsedNum();
            }
        }
        vo.setCouponPublishNum(publish);
        vo.setCouponReceivedNum(received);
        vo.setCouponUsedNum(used);

        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        Double discount = orderStatisticsService.getDiscountPrice(dates);
        vo.setDiscountAmount(discount == null ? 0D : discount);
        return vo;
    }

    /**
     * 按满额活动子类型统计当前进行中的活动数量（兼容未迁移的旧字段标志位）
     *
     * @param now      当前时间
     * @param typeEnum 满额活动子类型枚举
     * @return 进行中活动数量
     */
    private long countActiveFullDiscountByType(Date now, FullDiscountTypeEnum typeEnum) {
        QueryWrapper<FullDiscount> qw = Wrappers.query();
        qw.apply("start_time <= {0} AND (end_time IS NULL OR end_time >= {0})", now);
        qw.eq("delete_flag", false);
        if (FullDiscountTypeEnum.FULL_RATE == typeEnum) {
            qw.and(w -> w.eq("activity_type", FullDiscountTypeEnum.FULL_RATE.name())
                    .or(legacy -> legacy.isNull("activity_type").eq("full_rate_flag", true)));
        } else if (FullDiscountTypeEnum.FULL_GIFT == typeEnum) {
            qw.and(w -> w.eq("activity_type", FullDiscountTypeEnum.FULL_GIFT.name())
                    .or(legacy -> legacy.isNull("activity_type").eq("gift_flag", true)
                            .and(m -> m.isNull("full_minus_flag").or().eq("full_minus_flag", false))));
        } else {
            qw.and(w -> w.eq("activity_type", FullDiscountTypeEnum.FULL_MINUS.name())
                    .or(legacy -> legacy.isNull("activity_type").eq("full_minus_flag", true)));
        }
        return fullDiscountService.count(qw);
    }

    @SuppressWarnings({"rawtypes", "unchecked"})
    private long countActive(IService service, Date now) {
        QueryWrapper qw = Wrappers.query();
        qw.apply("start_time <= {0} AND (end_time IS NULL OR end_time >= {0})", now);
        qw.eq("delete_flag", false);
        return service.count(qw);
    }
}
