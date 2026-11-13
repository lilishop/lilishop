package cn.lili.modules.member.listener;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.member.event.MemberRegisterEvent;
import cn.lili.modules.member.service.MemberShareRegisterService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

/**
 * 客户注册成功事件监听器，解耦核心注册业务流程与邀请分享绑定、新人引导等附随逻辑。
 * 捕获并记录下游异常，保障核心注册及Token生成链路不被附随逻辑阻断。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class MemberRegisterListener {

    private final ObjectProvider<MemberShareRegisterService> memberShareRegisterServiceProvider;

    public MemberRegisterListener(ObjectProvider<MemberShareRegisterService> memberShareRegisterServiceProvider) {
        this.memberShareRegisterServiceProvider = memberShareRegisterServiceProvider;
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
        MemberShareRegisterService shareRegisterService = memberShareRegisterServiceProvider.getIfAvailable();
        if (shareRegisterService == null) {
            return;
        }
        try {
            shareRegisterService.bindRegisterSource(event.getMemberId(), event.getShareCode());
        } catch (Exception e) {
            log.error("客户注册后绑定分享来源异常，memberId: {}, shareCode: {}", event.getMemberId(), event.getShareCode(), e);
        }
    }
}
