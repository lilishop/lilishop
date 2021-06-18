package cn.lili.modules.promotion.mapper;

import cn.lili.modules.promotion.entity.dos.CouponActivityItem;
import cn.lili.modules.promotion.entity.vos.CouponActivityItemVO;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import java.util.List;

/**
 * 优惠券活动
 * @author Bulbasaur
 * @date: 2021/5/20 6:11 下午
 *
 */
public interface CouponActivityItemMapper extends BaseMapper<CouponActivityItem> {

    @Select("SELECT cai.*,c.coupon_name,c.price,c.coupon_type,c.coupon_discount FROM li_coupon_activity_item cai INNER JOIN li_coupon c ON cai.coupon_id = c.id WHERE cai.activity_id= #{activityId} ")
    List<CouponActivityItemVO> getCouponActivityItemListVO(String activityId);
}