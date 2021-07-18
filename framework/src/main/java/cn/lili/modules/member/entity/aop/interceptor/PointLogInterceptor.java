package cn.lili.modules.member.entity.aop.interceptor;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.service.MemberPointsHistoryService;
import cn.lili.modules.member.service.MemberService;
import lombok.extern.slf4j.Slf4j;
import org.aspectj.lang.JoinPoint;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.Aspect;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 积分操作切面
 *
 * @author Chopper
 * @date 2020/11/17 7:22 下午
 */
@Slf4j
@Aspect
@Component
public class PointLogInterceptor {

    @Autowired
    private MemberPointsHistoryService memberPointsHistoryService;

    @Autowired
    private MemberService memberService;

    @After("@annotation(cn.lili.modules.member.entity.aop.annotation.PointLogPoint)")
    public void doAfter(JoinPoint pjp) {
        //参数
        Object[] obj = pjp.getArgs();
        try {
            //变动积分
            Long point = 0L;
            if (obj[0] != null) {
                point = Long.valueOf(obj[0].toString());
            }
            //变动类型
            Boolean type = false;
            if (obj[1] != null) {
                type = Boolean.valueOf(obj[1].toString());
            }
            //会员ID
            String memberId = "";
            if (obj[2] != null) {
                memberId = obj[2].toString();
            }
            //根据会员id查询会员信息
            Member member = memberService.getById(memberId);
            if (member != null) {
                MemberPointsHistory memberPointsHistory = new MemberPointsHistory();
                memberPointsHistory.setMemberId(member.getId());
                memberPointsHistory.setMemberName(member.getUsername());
                memberPointsHistory.setPointType(type ? PointTypeEnum.INCREASE.name() : PointTypeEnum.REDUCE.name());

                memberPointsHistory.setVariablePoint(point);
                memberPointsHistory.setBeforePoint(true ? new Double(CurrencyUtil.sub(member.getPoint(), point)).longValue() : new Double(CurrencyUtil.add(member.getPoint(), point)).longValue());
                memberPointsHistory.setPoint(member.getPoint());
                memberPointsHistory.setContent(obj[3] == null ? "" : obj[3].toString());
                memberPointsHistory.setCreateBy("系统");
                memberPointsHistoryService.save(memberPointsHistory);
            }
        } catch (Exception e) {
            log.error("积分操作错误", e);
        }


    }

}
