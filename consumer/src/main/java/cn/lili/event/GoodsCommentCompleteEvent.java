package cn.lili.event;

import cn.lili.modules.member.entity.dos.MemberEvaluation;

/**
 * 订单状态改变事件
 *
 * @author Chopper
 * @since 2020/11/17 7:13 下午
 */
public interface GoodsCommentCompleteEvent {

    /**
     * 商品评价
     * @param memberEvaluation 会员评价
     */
    void goodsComment(MemberEvaluation memberEvaluation);
}
