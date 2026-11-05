package cn.lili.timetask.handler.impl.coupon;

import cn.lili.modules.member.service.MemberBenefitService;
import cn.lili.timetask.handler.EveryHourExecute;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 客户权益小时任务入口，委托权益服务执行当前周期发放及失败恢复，发放账本负责来源和周期防重。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Component
public class MemberBenefitExecute implements EveryHourExecute {
    /**
     * 调用权益服务扫描普通等级及有效付费会员订单，处理当前周期权益并恢复符合条件的待发放或失败记录。
     * 周期计算、来源资格校验、单项事务及防重由权益服务及发放账本实现。
     */
    /**
     * 统一的权益周期发放与失败恢复入口。
     */
    @Autowired private MemberBenefitService memberBenefitService;
    @Override public void execute() { memberBenefitService.processScheduledBenefits(); }
}
