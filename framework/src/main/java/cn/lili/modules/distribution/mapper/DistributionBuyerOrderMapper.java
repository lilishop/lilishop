package cn.lili.modules.distribution.mapper;

import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderSummaryVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerOrderVO;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.Date;
import java.util.List;

/**
 * 分销员端推广/邀请订单查询
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionBuyerOrderMapper {

    /** 关键词筛选：订单号、商品名、买家手机号 */
    String KEYWORD_FILTER =
            "<if test='keyword != null and keyword != \"\"'>"
                    + " AND (do.order_sn LIKE CONCAT('%', #{keyword}, '%') "
                    + " OR do.goods_name LIKE CONCAT('%', #{keyword}, '%') "
                    + " OR m.mobile LIKE CONCAT('%', #{keyword}, '%')) "
                    + "</if>";

    /** 下单时间范围筛选 */
    String TIME_FILTER =
            "<if test='startTime != null'> AND do.create_time &gt;= #{startTime} </if>"
                    + "<if test='endTime != null'> AND do.create_time &lt;= #{endTime} </if>";

    /** 一级推广佣金（扣除退款冲回） */
    String DIRECT_COMMISSION_EXPR =
            "CASE WHEN do.distribution_order_status = 'REFUND' THEN 0 "
                    + "ELSE GREATEST(IFNULL(do.rebate, 0) - IFNULL(do.sell_back_rebate, 0), 0) END";

    /** 二级邀请佣金（扣除退款冲回） */
    String INVITE_COMMISSION_EXPR =
            "CASE WHEN do.distribution_order_status = 'REFUND' THEN 0 "
                    + "ELSE GREATEST(IFNULL(do.invite_commission, 0) - IFNULL(do.invite_sell_back_rebate, 0), 0) END";

    /** 结算状态：SETTLED / PENDING / NOT_SETTLE */
    String SETTLEMENT_TYPE_EXPR =
            "CASE WHEN do.distribution_order_status = 'REFUND' THEN 'NOT_SETTLE' "
                    + "WHEN do.distribution_order_status = 'COMPLETE' THEN 'SETTLED' "
                    + "WHEN do.distribution_order_status = 'NO_COMPLETED' THEN 'PENDING' "
                    + "ELSE 'NOT_SETTLE' END";

    /** 是否已退款 */
    String REFUNDED_EXPR =
            "CASE WHEN do.distribution_order_status = 'REFUND' THEN 1 ELSE 0 END";

    /** 推广订单列表查询字段 */
    String PROMOTION_SELECT_FIELDS =
            "do.order_sn AS orderSn, do.order_item_sn AS orderItemSn, do.member_name AS memberName, "
                    + "do.create_time AS createTime, do.goods_name AS goodsName, do.image AS image, "
                    + "COALESCE(NULLIF(gs.simple_specs, ''), do.specs) AS specs, do.num AS num, sf.final_price AS finalPrice, "
                    + "(" + DIRECT_COMMISSION_EXPR + ") AS commissionAmount, "
                    + "(" + SETTLEMENT_TYPE_EXPR + ") AS settlementStatusType, "
                    + "(" + REFUNDED_EXPR + ") AS refunded, do.settle_time AS settleTime ";

    /** 邀请订单列表查询字段 */
    String INVITE_SELECT_FIELDS =
            "do.order_sn AS orderSn, do.order_item_sn AS orderItemSn, do.member_name AS memberName, "
                    + "do.create_time AS createTime, do.goods_name AS goodsName, do.image AS image, "
                    + "COALESCE(NULLIF(gs.simple_specs, ''), do.specs) AS specs, do.num AS num, sf.final_price AS finalPrice, "
                    + "(" + INVITE_COMMISSION_EXPR + ") AS commissionAmount, "
                    + "(" + SETTLEMENT_TYPE_EXPR + ") AS settlementStatusType, "
                    + "(" + REFUNDED_EXPR + ") AS refunded, do.settle_time AS settleTime ";

    /** 推广订单列表 FROM 子句 */
    String PROMOTION_FROM =
            "FROM li_distribution_order do "
                    + "LEFT JOIN li_member m ON m.id = do.member_id AND m.delete_flag = 0 "
                    + "LEFT JOIN li_goods_sku gs ON gs.id = do.sku_id AND gs.delete_flag = 0 "
                    + "LEFT JOIN li_store_flow sf ON sf.order_item_sn = do.order_item_sn AND sf.flow_type = 'PAY' "
                    + "WHERE do.distribution_id = #{distributionId} "
                    + TIME_FILTER + KEYWORD_FILTER;

    /** 邀请订单列表 FROM 子句 */
    String INVITE_FROM =
            "FROM li_distribution_order do "
                    + "LEFT JOIN li_member m ON m.id = do.member_id AND m.delete_flag = 0 "
                    + "LEFT JOIN li_goods_sku gs ON gs.id = do.sku_id AND gs.delete_flag = 0 "
                    + "LEFT JOIN li_store_flow sf ON sf.order_item_sn = do.order_item_sn AND sf.flow_type = 'PAY' "
                    + "WHERE do.invite_distribution_id = #{distributionId} "
                    + "AND IFNULL(do.invite_commission, 0) &gt; 0 "
                    + TIME_FILTER + KEYWORD_FILTER;

    /** 订单详情额外字段（关联主订单） */
    String DETAIL_EXTRA =
            ", o.flow_price AS totalOrderPrice, o.goods_num AS totalGoodsNum, "
                    + "o.payment_time AS paymentTime, o.order_status AS orderStatus, "
                    + "o.pay_status AS payStatus, o.deliver_status AS deliverStatus, "
                    + "o.delivery_method AS deliveryMethod, o.logistics_name AS logisticsName, "
                    + "o.logistics_no AS logisticsNo ";

    /** 推广订单详情 FROM 子句 */
    String PROMOTION_DETAIL_FROM =
            "FROM li_distribution_order do "
                    + "INNER JOIN li_order o ON o.sn = do.order_sn AND o.delete_flag = 0 "
                    + "LEFT JOIN li_member m ON m.id = do.member_id AND m.delete_flag = 0 "
                    + "LEFT JOIN li_goods_sku gs ON gs.id = do.sku_id AND gs.delete_flag = 0 "
                    + "LEFT JOIN li_store_flow sf ON sf.order_item_sn = do.order_item_sn AND sf.flow_type = 'PAY' "
                    + "WHERE do.distribution_id = #{distributionId} AND do.order_item_sn = #{orderItemSn}";

    /** 邀请订单详情 FROM 子句 */
    String INVITE_DETAIL_FROM =
            "FROM li_distribution_order do "
                    + "INNER JOIN li_order o ON o.sn = do.order_sn AND o.delete_flag = 0 "
                    + "LEFT JOIN li_member m ON m.id = do.member_id AND m.delete_flag = 0 "
                    + "LEFT JOIN li_goods_sku gs ON gs.id = do.sku_id AND gs.delete_flag = 0 "
                    + "LEFT JOIN li_store_flow sf ON sf.order_item_sn = do.order_item_sn AND sf.flow_type = 'PAY' "
                    + "WHERE do.invite_distribution_id = #{distributionId} "
                    + "AND do.order_item_sn = #{orderItemSn}";

    /**
     * 汇总推广订单数量与佣金合计。
     *
     * @param distributionId 分销员 ID
     * @param startTime        下单开始时间（可空）
     * @param endTime          下单结束时间（可空）
     * @param keyword          关键词（可空）
     * @return 总数与佣金合计
     */
    @Select("<script>SELECT COUNT(1) AS total, COALESCE(SUM(" + DIRECT_COMMISSION_EXPR + "), 0) AS totalCommission "
            + PROMOTION_FROM + "</script>")
    DistributionBuyerOrderSummaryVO summarizePromotionOrders(@Param("distributionId") String distributionId,
                                                             @Param("startTime") Date startTime,
                                                             @Param("endTime") Date endTime,
                                                             @Param("keyword") String keyword);

    /**
     * 分页查询推广订单列表。
     *
     * @param distributionId 分销员 ID
     * @param startTime        下单开始时间（可空）
     * @param endTime          下单结束时间（可空）
     * @param keyword          关键词（可空）
     * @param offset           分页偏移
     * @param pageSize         每页条数
     * @return 推广订单列表
     */
    @Select("<script>SELECT " + PROMOTION_SELECT_FIELDS + PROMOTION_FROM
            + " ORDER BY do.create_time DESC LIMIT #{offset}, #{pageSize}</script>")
    List<DistributionBuyerOrderVO> listPromotionOrders(@Param("distributionId") String distributionId,
                                                       @Param("startTime") Date startTime,
                                                       @Param("endTime") Date endTime,
                                                       @Param("keyword") String keyword,
                                                       @Param("offset") long offset,
                                                       @Param("pageSize") long pageSize);

    /**
     * 汇总邀请订单数量与邀请佣金合计。
     *
     * @param distributionId 分销员 ID
     * @param startTime        下单开始时间（可空）
     * @param endTime          下单结束时间（可空）
     * @param keyword          关键词（可空）
     * @return 总数与佣金合计
     */
    @Select("<script>SELECT COUNT(1) AS total, COALESCE(SUM(" + INVITE_COMMISSION_EXPR + "), 0) AS totalCommission "
            + INVITE_FROM + "</script>")
    DistributionBuyerOrderSummaryVO summarizeInviteOrders(@Param("distributionId") String distributionId,
                                                          @Param("startTime") Date startTime,
                                                          @Param("endTime") Date endTime,
                                                          @Param("keyword") String keyword);

    /**
     * 分页查询邀请订单列表。
     *
     * @param distributionId 分销员 ID
     * @param startTime        下单开始时间（可空）
     * @param endTime          下单结束时间（可空）
     * @param keyword          关键词（可空）
     * @param offset           分页偏移
     * @param pageSize         每页条数
     * @return 邀请订单列表
     */
    @Select("<script>SELECT " + INVITE_SELECT_FIELDS + INVITE_FROM
            + " ORDER BY do.create_time DESC LIMIT #{offset}, #{pageSize}</script>")
    List<DistributionBuyerOrderVO> listInviteOrders(@Param("distributionId") String distributionId,
                                                    @Param("startTime") Date startTime,
                                                    @Param("endTime") Date endTime,
                                                    @Param("keyword") String keyword,
                                                    @Param("offset") long offset,
                                                    @Param("pageSize") long pageSize);

    /**
     * 查询推广订单详情。
     *
     * @param distributionId 分销员 ID
     * @param orderItemSn    子订单编号
     * @return 订单详情，不存在时返回 null
     */
    @Select("SELECT " + PROMOTION_SELECT_FIELDS + DETAIL_EXTRA + PROMOTION_DETAIL_FROM + " LIMIT 1")
    DistributionBuyerOrderDetailVO getPromotionOrderDetail(@Param("distributionId") String distributionId,
                                                           @Param("orderItemSn") String orderItemSn);

    /**
     * 查询邀请订单详情。
     *
     * @param distributionId 分销员 ID
     * @param orderItemSn    子订单编号
     * @return 订单详情，不存在时返回 null
     */
    @Select("SELECT " + INVITE_SELECT_FIELDS + DETAIL_EXTRA + INVITE_DETAIL_FROM + " LIMIT 1")
    DistributionBuyerOrderDetailVO getInviteOrderDetail(@Param("distributionId") String distributionId,
                                                        @Param("orderItemSn") String orderItemSn);
}
