package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.MemberCouponSign;
import cn.lili.modules.promotion.entity.vos.CouponActivityVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 优惠券领取标记业务层
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2023/1/3 18:12
 */

public interface MemberCouponSignService extends IService<MemberCouponSign> {


    /**
     * 清除缓存
     * 清除失效标记
     */
    void clean();


    /**
     * 清除缓存
     * 清除失效标记
     */
    List<CouponActivityVO> receiveCoupon(List<CouponActivityVO> couponActivity);
}