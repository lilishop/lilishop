package cn.lili.modules.member.service;

/**
 * 购物积分业务接口，由交易完结处理在交易完结后按净金额赠送，复用积分流水防重，不增加订单字段。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
public interface MemberOrderPointService {

    /**
     * 交易完结处理确认订单已完成、售后入口已关闭且全部售后终结后直接赠送积分。
     * 使用持久化订单金额、实际退款及售后状态，按发放时当前积分设置和当前有效权益最高倍率计算。
     * @return true 表示已赠送、已发过或无需赠送；false 表示尚未满足交易完结条件
     * @param orderSn 订单编号，用于读取数据库中的最新订单及售后数据
     */
    boolean complete(String orderSn);
}
