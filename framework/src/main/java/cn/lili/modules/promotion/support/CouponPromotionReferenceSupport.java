package cn.lili.modules.promotion.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.promotion.entity.dos.FullDiscount;
import cn.lili.modules.promotion.entity.dto.search.FullDiscountSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.mapper.CouponMapper;
import cn.lili.modules.promotion.mapper.FullDiscountMapper;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 优惠券与满额活动的关联校验，统一优惠券存在性及进行中活动引用检查，避免两个业务服务相互依赖。
 * 只读取关联数据，不发放优惠券或修改活动，沿用调用方事务和原有查询条件。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class CouponPromotionReferenceSupport {

    private final CouponMapper couponMapper;
    private final FullDiscountMapper fullDiscountMapper;

    /**
     * 注入优惠券和满额活动存储，不依赖对应业务服务。
     *
     * @param couponMapper 优惠券存储
     * @param fullDiscountMapper 满额活动存储
     */
    public CouponPromotionReferenceSupport(CouponMapper couponMapper, FullDiscountMapper fullDiscountMapper) {
        this.couponMapper = couponMapper;
        this.fullDiscountMapper = fullDiscountMapper;
    }

    /**
     * 检查满额活动选择的优惠券是否存在，沿用存储层逻辑删除规则。
     * 不额外限制优惠券状态、领取数量、所属店铺或有效期。
     *
     * @param couponId 优惠券主键
     * @throws ServiceException 未选择优惠券或优惠券不存在时抛出
     */
    public void checkCouponExists(String couponId) {
        if (couponId == null || couponMapper.selectById(couponId) == null) {
            throw new ServiceException(ResultCode.COUPON_NOT_EXIST);
        }
    }

    /**
     * 编辑或删除优惠券前，阻止修改正在进行的满额活动所赠送的优惠券。
     * 复用原查询的赠券标志、优惠券ID、START状态及未删除条件，不额外限定店铺。
     *
     * @param couponId 待编辑或删除的优惠券主键
     * @throws ServiceException 存在引用时返回首个匹配活动名称并阻止操作
     */
    public void checkCouponNotUsedByActiveFullDiscount(String couponId) {
        FullDiscountSearchParams searchParams = new FullDiscountSearchParams();
        searchParams.setCouponFlag(true);
        searchParams.setCouponId(couponId);
        searchParams.setPromotionStatus(PromotionsStatusEnum.START.name());
        List<FullDiscount> fullDiscounts = fullDiscountMapper.selectList(searchParams.queryWrapper());
        if (fullDiscounts != null && !fullDiscounts.isEmpty()) {
            throw new ServiceException("当前优惠券参与了促销活动【" + fullDiscounts.get(0).getPromotionName() + "】不能进行编辑删除操作");
        }
    }
}
