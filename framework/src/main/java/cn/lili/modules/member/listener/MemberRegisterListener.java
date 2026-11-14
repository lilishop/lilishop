package cn.lili.modules.member.listener;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.member.event.MemberRegisterEvent;
import cn.lili.modules.member.support.MemberShareSourceSupport;
import lombok.extern.slf4j.Slf4j;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * 客户注册成功事件监听器，解耦核心注册流程与邀请来源绑定，不负责经验奖励发放。
 * 捕获并记录下游异常，保障核心注册及Token生成链路不被附随逻辑阻断。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class MemberRegisterListener {

    private final MemberShareSourceSupport memberShareSourceSupport;

    /**
     * 注入仅处理邀请来源的必需组件，避免为绑定来源依赖完整分享奖励服务。
     *
     * @param memberShareSourceSupport 邀请来源绑定组件
     */
    public MemberRegisterListener(MemberShareSourceSupport memberShareSourceSupport) {
        this.memberShareSourceSupport = memberShareSourceSupport;
    }

    /**
     * 响应客户注册事件，当存在有效分享邀请码时记录来源关系。
     *
     * @param event 客户注册成功事件
     */
    @EventListener
    public void onMemberRegister(MemberRegisterEvent event) {
        if (event == null || CharSequenceUtil.isBlank(event.getMemberId())) {
            return;
        }
        if (CharSequenceUtil.isBlank(event.getShareCode())) {
            return;
        }
        try {
            memberShareSourceSupport.bindRegisterSource(event.getMemberId(), event.getShareCode());
        } catch (Exception e) {
            log.error("客户注册后绑定分享来源异常，memberId: {}, shareCode: {}", event.getMemberId(), event.getShareCode(), e);
        }
    }
}
