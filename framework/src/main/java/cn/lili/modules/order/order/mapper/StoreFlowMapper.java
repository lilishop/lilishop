package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.store.entity.dos.Bill;
import cn.lili.modules.store.entity.vos.StoreFlowPayDownloadVO;
import cn.lili.modules.store.entity.vos.StoreFlowRefundDownloadVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

/**
 * 商家订单流水数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 7:35 下午
 */
public interface StoreFlowMapper extends BaseMapper<StoreFlow> {

    /**
     * 获取结算单的入账流水
     * @param queryWrapper 查询条件
     * @return 入账流水
     */
    @Select("SELECT * FROM li_store_flow ${ew.customSqlSegment}")
    List<StoreFlowPayDownloadVO> getStoreFlowPayDownloadVO(@Param(Constants.WRAPPER) Wrapper<StoreFlow> queryWrapper);

    /**
     * 获取结算单的退款流水
     * @param queryWrapper 查询条件
     * @return 退款流水
     */
    @Select("SELECT * FROM li_store_flow ${ew.customSqlSegment}")
    List<StoreFlowRefundDownloadVO> getStoreFlowRefundDownloadVO(@Param(Constants.WRAPPER) Wrapper<StoreFlow> queryWrapper);

    /**
     * 获取本次待完结流水；订单须完成、所有商品项不可售后且不存在处理中售后。
     * 仅返回 WAIT_COMPLETE，避免将同一商品项其他状态的流水重新结算。
     * @return 符合条件的支付及退款流水，按订单号分组处理积分
     */
    @Select("SELECT f.* FROM li_store_flow f WHERE f.profit_sharing_status='WAIT_COMPLETE' "
            + "AND EXISTS (SELECT 1 FROM li_order o WHERE o.sn=f.order_sn AND o.delete_flag=0 AND o.order_status='COMPLETED') "
            + "AND EXISTS (SELECT 1 FROM li_order_item i WHERE i.order_sn=f.order_sn) "
            + "AND NOT EXISTS (SELECT 1 FROM li_order_item i WHERE i.order_sn=f.order_sn AND (i.after_sale_status IS NULL OR i.after_sale_status<>'EXPIRED')) "
            + "AND NOT EXISTS (SELECT 1 FROM li_after_sale a WHERE a.order_sn=f.order_sn AND (a.service_status IS NULL "
            + "OR a.service_status NOT IN ('COMPLETE','REFUSE','SELLER_TERMINATION','BUYER_CANCEL')))")
    List<StoreFlow> completeList();


    /**
     * 查询订单货物退款数量
     * @param orderItemSn
     * @return
     */
    @Select("SELECT SUM(num) AS num FROM li_store_flow WHERE flow_type = 'REFUND' AND order_item_sn = #{orderItemSn}")
    Integer getRefundNum(String orderItemSn);


    /**
     * 查询退款结算单
     *
     * @param queryWrapper 查询条件
     * @return 结算单
     */
    @Select("SELECT IFNULL(SUM( final_price ),0) AS refundPrice,IFNULL(SUM( commission_price ),0) AS refundCommissionPrice" +
            ",IFNULL(SUM( distribution_rebate ),0) AS distributionRefundCommission" +
            ",IFNULL(SUM( site_coupon_commission ),0) AS siteCouponRefundCommission" +
            ",IFNULL(SUM( gift_card_subsidy_price ),0) AS giftCardRefundSubsidy" +
            ",IFNULL(SUM( point_settlement_price ),0) AS pointSettlementPrice " +
            ",IFNULL(SUM( kanjia_settlement_price ),0) AS kanjiaSettlementPrice " +
            ",IFNULL(SUM( bill_price ),0) AS billPrice " +
            "FROM li_store_flow ${ew.customSqlSegment}")
    Bill getRefundBill(@Param(Constants.WRAPPER) QueryWrapper<Bill> queryWrapper);


    /**
     * 查询订单结算
     *
     * @param queryWrapper 查询条件
     * @return 结算单
     */
    @Select("SELECT IFNULL(SUM( final_price ),0) AS orderPrice" +
            ",IFNULL(SUM( commission_price ),0) AS commissionPrice" +
            ",IFNULL(SUM( distribution_rebate ),0) AS distributionCommission" +
            ",IFNULL(SUM( site_coupon_commission ),0) AS siteCouponCommission" +
            ",IFNULL(SUM( gift_card_subsidy_price ),0) AS giftCardSubsidy" +
            ",IFNULL(SUM( point_settlement_price ),0) AS pointSettlementPrice " +
            ",IFNULL(SUM( kanjia_settlement_price ),0) AS kanjiaSettlementPrice " +
            ",IFNULL(SUM( bill_price ),0) AS billPrice " +
            "FROM li_store_flow ${ew.customSqlSegment}")
    Bill getOrderBill(@Param(Constants.WRAPPER) QueryWrapper<Bill> queryWrapper);
}