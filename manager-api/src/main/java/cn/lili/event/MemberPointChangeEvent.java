package cn.lili.event;

import cn.lili.modules.member.entity.dto.MemberPointMessage;

/**
 * 客户积分改变消息
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface MemberPointChangeEvent {

    /**
     * 客户积分改变消息
     *
     * @param memberPointMessage 客户积分消息
     */
    void memberPointChange(MemberPointMessage memberPointMessage);
}
