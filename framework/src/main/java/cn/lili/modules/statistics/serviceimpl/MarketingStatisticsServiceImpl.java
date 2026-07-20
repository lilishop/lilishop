package cn.lili.modules.statistics.serviceimpl;

import cn.lili.modules.promotion.entity.dos.Coupon;
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

    @SuppressWarnings({"rawtypes", "unchecked"})
    private long countActive(IService service, Date now) {
        QueryWrapper qw = Wrappers.query();
        qw.apply("start_time <= {0} AND (end_time IS NULL OR end_time >= {0})", now);
        qw.eq("delete_flag", false);
        return service.count(qw);
    }
}
