package cn.lili.modules.promotion.mapper;

import cn.lili.modules.promotion.entity.dos.Coupon;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Update;
import org.apache.ibatis.annotations.Select;

/**
 * 优惠券数据处理层
 *
 * @author Chopper
 * @since 2020-08-21 11:46
 */
public interface CouponMapper extends BaseMapper<Coupon> {

    /**
     * 在发奖事务内锁定券模板，保护归属、有效期及发行库存的当前读取。
     *
     * @param id 券模板ID
     * @return 未删除的当前券模板
     */
    @Select("SELECT * FROM li_coupon WHERE id=#{id} AND delete_flag=0 FOR UPDATE")
    Coupon lockCoupon(@Param("id") String id);

    /**
     * 原子增加已领取数量，带发行量超发保护。
     * 当 publish_num != 0 时，必须保证 received_num + receiveNum <= publish_num；
     * 当 publish_num == 0 时表示不限量发行。
     * 若超过发行量，更新影响行数为 0。
     *
     * @param couponId 优惠券ID
     * @param receiveNum 领取数量
     * @return 影响行数
     */
    @Update("UPDATE li_coupon SET received_num = received_num + #{receiveNum} " +
            "WHERE id = #{couponId} AND (publish_num = 0 OR received_num + #{receiveNum} <= publish_num)")
    int increaseReceivedNum(@Param("couponId") String couponId, @Param("receiveNum") Integer receiveNum);

}
