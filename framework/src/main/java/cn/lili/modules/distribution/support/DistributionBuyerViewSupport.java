package cn.lili.modules.distribution.support;

import cn.hutool.core.bean.BeanUtil;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;

import java.math.BigDecimal;

/**
 * 买家端分销员响应视图。
 * <p>
 * 清退记录必须返回对象供入口按状态路由；金额与业绩只在拷贝上脱敏，不写库。
 * </p>
 *
 * @author Mike
 * @date 2026-09-09
 */
public final class DistributionBuyerViewSupport {

    private DistributionBuyerViewSupport() {
    }

    /**
     * 组装买家 GET /distribution 响应。
     * <p>
     * 无记录返回 null；清退返回脱敏拷贝（保留 id / memberId / distributionStatus）；
     * 其余状态返回原对象。调用方不得对返回对象执行 updateById。
     * </p>
     *
     * @param distribution 当前会员分销员记录，可为 null
     * @return 买家可见视图
     */
    public static Distribution forBuyerResponse(Distribution distribution) {
        if (distribution == null) {
            return null;
        }
        if (!DistributionStatusEnum.RETREAT.name().equals(distribution.getDistributionStatus())) {
            return distribution;
        }
        return maskRetreatedCopy(distribution);
    }

    /**
     * 拷贝后抹掉金额与业绩展示字段，避免把库内真实余额带给前端。
     *
     * @param source 库内清退记录
     * @return 脱敏拷贝
     */
    private static Distribution maskRetreatedCopy(Distribution source) {
        Distribution view = new Distribution();
        BeanUtil.copyProperties(source, view);
        view.setCanRebate(0D);
        view.setCommissionFrozen(0D);
        view.setRebateTotal(0D);
        view.setWithdrawalFrozen(BigDecimal.ZERO);
        view.setWithdrawnTotal(BigDecimal.ZERO);
        view.setDistributionOrderCount(0);
        view.setDistributionOrderPrice(0D);
        view.setValidSalesAmount(BigDecimal.ZERO);
        view.setValidCommissionAmount(BigDecimal.ZERO);
        view.setValidCustomerCount(0);
        view.setInviteCount(0);
        return view;
    }
}
