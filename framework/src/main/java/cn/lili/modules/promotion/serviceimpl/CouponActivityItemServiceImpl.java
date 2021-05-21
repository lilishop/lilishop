package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.mapper.CouponActivityItemMapper;
import cn.lili.modules.promotion.service.CouponActivityItemService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 优惠券活动关联优惠券业务层实现
 *
 * @author Bulbasaur
 * @date: 2021/5/21 6:42 下午
 */
@Service
public class CouponActivityItemServiceImpl extends ServiceImpl<CouponActivityItemMapper, CouponActivityItem> implements CouponActivityItemService {
    @Override
    public List<CouponActivityItem> getCouponActivityList(String activityId) {
        return this.list(this.lambdaQuery().eq(CouponActivityItem::getActivityId, activityId));
    }
}
