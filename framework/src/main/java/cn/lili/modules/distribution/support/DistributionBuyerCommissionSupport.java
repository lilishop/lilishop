package cn.lili.modules.distribution.support;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsItemVO;
import cn.lili.modules.distribution.entity.vos.DistributionGoodsVO;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.util.DistributionCommissionCalculator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;
import java.util.List;

/**
 * 买家端展示佣金按等级比例换算
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Component
public class DistributionBuyerCommissionSupport {

    @Autowired
    private DistributionGradeService distributionGradeService;

    /**
     * 解析分销员等级对应的商品佣金比例。
     *
     * @param gradeId 等级ID或等级序号
     * @return 商品佣金比例
     */
    public BigDecimal resolveGoodsCommissionRate(String gradeId) {
        // 查不到等级时回退到零佣金占位，避免 NPE
        DistributionGrade grade = distributionGradeService.resolveGradeWithFallback(gradeId);
        return grade.getGoodsCommissionRate() == null ? BigDecimal.ZERO : grade.getGoodsCommissionRate();
    }

    /**
     * 按等级比例计算一级商品佣金。
     *
     * @param pool    佣金池
     * @param gradeId 等级ID或等级序号
     * @return 一级佣金金额
     */
    public BigDecimal calculateDirectCommission(BigDecimal pool, String gradeId) {
        return DistributionCommissionCalculator.calculateDirectCommission(pool, resolveGoodsCommissionRate(gradeId));
    }

    /**
     * 按等级比例换算展示用佣金（Double）。
     *
     * @param pool    佣金池
     * @param gradeId 等级ID或等级序号
     * @return 展示佣金
     */
    public Double applyDirectCommission(Double pool, String gradeId) {
        if (pool == null || pool <= 0) {
            return 0D;
        }
        // item.commission 存的是佣金池，按等级比例换算为展示佣金
        return calculateDirectCommission(BigDecimal.valueOf(pool), gradeId).doubleValue();
    }

    /**
     * 为主推商品列表按分销员等级换算展示佣金。
     *
     * @param items        主推商品列表
     * @param distribution 当前分销员
     */
    public void applyToFeaturedGoods(List<DistributionFeaturedGoodsItemVO> items, Distribution distribution) {
        if (distribution == null || items == null || items.isEmpty()) {
            return;
        }
        String gradeId = distribution.getGradeId();
        // 就地覆写列表项佣金，供买家端按当前等级展示
        items.forEach(item -> item.setCommission(applyDirectCommission(item.getCommission(), gradeId)));
    }

    /**
     * 为分销商品列表按分销员等级换算展示佣金。
     *
     * @param items        分销商品列表
     * @param distribution 当前分销员
     */
    public void applyToGoods(List<DistributionGoodsVO> items, Distribution distribution) {
        if (distribution == null || items == null || items.isEmpty()) {
            return;
        }
        String gradeId = distribution.getGradeId();
        items.forEach(item -> item.setCommission(applyDirectCommission(item.getCommission(), gradeId)));
    }
}
