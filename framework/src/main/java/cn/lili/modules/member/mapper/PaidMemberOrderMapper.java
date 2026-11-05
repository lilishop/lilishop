package cn.lili.modules.member.mapper;
import cn.lili.modules.member.entity.dos.PaidMemberOrder;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
/**
 * 付费会员订单数据层。
 * @author Bulbasaur
 * @since 2026-11-04
 */
public interface PaidMemberOrderMapper extends BaseMapper<PaidMemberOrder> {
    /**
     * 按客户累计汇总商品会员优惠，跨开通/续费周期统计，扣除退款流水冲回的平台会员补贴。
     * li_store_flow 为交易流水表，不包含逻辑删除字段；按流水类型计算实际累计优惠。
     * @param memberId 客户 ID，对应 li_store_flow.member_id，仅统计该客户的支付和退款流水
     * @return 已支付商品的净会员优惠金额，单位元；没有相关流水时为 0
     */
    @org.apache.ibatis.annotations.Select("SELECT COALESCE(SUM(CASE WHEN flow_type='PAY' THEN paid_member_subsidy_price ELSE -paid_member_subsidy_price END),0) FROM li_store_flow WHERE member_id=#{memberId} AND flow_type IN ('PAY','REFUND')")
    java.math.BigDecimal savedByMember(@org.apache.ibatis.annotations.Param("memberId") String memberId);
}
