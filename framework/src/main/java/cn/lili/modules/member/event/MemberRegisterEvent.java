package cn.lili.modules.member.event;

import cn.lili.modules.member.entity.dos.Member;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import org.springframework.context.ApplicationEvent;

/**
 * 客户注册成功事件，用于通过Spring事件机制解耦注册后续链路（如绑定分享邀请来源、发放新人权益或MQ同步）。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Getter
public class MemberRegisterEvent extends ApplicationEvent {

    private static final long serialVersionUID = 1L;

    @Schema(description = "已持久化的新客户实体")
    private final Member member;

    @Schema(description = "新客户ID，关联li_member主键")
    private final String memberId;

    @Schema(description = "注册时填写的邀请码或分销分享码，无邀请人时为null")
    private final String shareCode;

    /**
     * 构造客户注册成功事件。
     *
     * @param source    事件发布源对象
     * @param member    新注册客户实体
     * @param shareCode 注册填写的邀请分享码
     */
    public MemberRegisterEvent(Object source, Member member, String shareCode) {
        super(source);
        this.member = member;
        this.memberId = member != null ? member.getId() : null;
        this.shareCode = shareCode;
    }
}
