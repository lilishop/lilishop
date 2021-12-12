package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.entity.vos.CouponActivityItemVO;
import cn.lili.modules.promotion.mapper.CouponActivityItemMapper;
import cn.lili.modules.promotion.service.CouponActivityItemService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 优惠券活动关联优惠券业务层实现
 *
 * @author Bulbasaur
 * @since 2021/5/21 6:42 下午
 */
@Service
public class CouponActivityItemServiceImpl extends ServiceImpl<CouponActivityItemMapper, CouponActivityItem> implements CouponActivityItemService {

    @Override
    public List<CouponActivityItem> getCouponActivityList(String activityId) {
        LambdaQueryWrapper<CouponActivityItem> lambdaQueryWrapper = new LambdaQueryWrapper<>();
        lambdaQueryWrapper.eq(CouponActivityItem::getActivityId, activityId);
        return this.list(lambdaQueryWrapper);
    }

    @Override
    public List<CouponActivityItemVO> getCouponActivityItemListVO(String activityId) {
        return this.baseMapper.getCouponActivityItemListVO(activityId);
    }

    /**
     * 根据优惠券id删除优惠活动关联信息项
     *
     * @param couponIds 优惠券id集合
     */
    @Override
    public void removeByCouponId(List<String> couponIds) {
        this.remove(new LambdaQueryWrapper<CouponActivityItem>()
                .in(CouponActivityItem::getCouponId, couponIds));
    }
}
