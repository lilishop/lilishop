package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderExportDTO;
import cn.lili.modules.order.order.entity.dto.OrderVerificationRecordSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderNumVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.entity.vo.OrderVerificationRecordVO;
import cn.lili.modules.order.order.entity.vo.PaymentLog;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import java.util.List;

/**
 * 订单数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 7:35 下午
 */
public interface OrderMapper extends BaseMapper<Order> {

    /**
     * 修改订单状态
     *
     * @param status  状态
     * @param orderSn 订单编号
     */
    @Update({"update li_order set order_status = #{status} where sn = #{orderSn}"})
    void updateStatus(String status, String orderSn);

    /**
     * 查询导出订单DTO列表
     *
     * @param queryWrapper 查询条件
     * @return 导出订单DTO列表
     */
    @Select("SELECT o.sn AS order_sn," +
            "oi.sn AS order_item_sn," +
            "oi.goods_name AS goods_name," +
            "oi.num AS num," +
            "oi.goods_id AS goods_id," +
            "oi.unit_price AS unit_price," +
            "oi.flow_price AS flow_price," +
            "oi.price_detail AS price_detail," +
            "o.payment_method AS payment_method," +
            "o.consignee_name AS consignee_name," +
            "o.consignee_mobile AS consignee_mobile," +
            "o.consignee_address_path AS consignee_address_path," +
            "o.consignee_detail AS consignee_detail," +
            "o.remark AS remark," +
            "o.create_time AS create_time," +
            "o.payment_time AS payment_time," +
            "o.client_type AS client_type," +
            "o.order_status AS order_status," +
            "o.order_type AS order_type," +
            "oi.after_sale_status AS after_sale_status," +
            "o.logistics_time AS logistics_time," +
            "o.complete_time AS complete_time," +
            "o.store_name AS store_name " +
            " FROM li_order o LEFT JOIN li_order_item oi ON oi.order_sn = o.sn ${ew.customSqlSegment}")
    List<OrderExportDTO> queryExportOrder(@Param(Constants.WRAPPER) Wrapper<OrderSimpleVO> queryWrapper);

    /**
     * 查询订单支付记录
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 订单支付记录分页
     */
    @Select("select * from li_order ${ew.customSqlSegment} ")
    IPage<PaymentLog> queryPaymentLogs(IPage<PaymentLog> page, @Param(Constants.WRAPPER) Wrapper<PaymentLog> queryWrapper);

    /**
     * 买家订单列表。只返回买家页面用到的订单和商品字段。
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 买家订单分页
     */
    @Select("select o.sn,o.flow_price,o.create_time,o.order_status,o.pay_status,o.payment_method,o.store_name as store_name,o.store_id as store_id," +
            "o.order_type,o.deliver_status,o.order_promotion_type" +
            ",GROUP_CONCAT(oi.goods_id) as group_goods_id," +
            " GROUP_CONCAT(oi.sku_id) as group_sku_id," +
            " GROUP_CONCAT(oi.num) as group_num" +
            ",GROUP_CONCAT(oi.image) as group_images" +
            ",GROUP_CONCAT(oi.goods_name) as group_name " +
            ",GROUP_CONCAT(oi.after_sale_status) as group_after_sale_status" +
            ",GROUP_CONCAT(oi.complain_status) as group_complain_status" +
            ",GROUP_CONCAT(oi.comment_status) as group_comment_status" +
            ",GROUP_CONCAT(oi.sn) as group_order_items_sn " +
            ",GROUP_CONCAT(oi.goods_price) as group_goods_price " +
            ",GROUP_CONCAT(oi.is_refund) as group_is_refund " +
            ",GROUP_CONCAT(oi.refund_price) as group_refund_price " +
            " FROM li_order o LEFT JOIN li_order_item AS oi on o.sn = oi.order_sn ${ew.customSqlSegment} ")
    IPage<OrderSimpleVO> queryBuyerByParams(IPage<OrderSimpleVO> page, @Param(Constants.WRAPPER) Wrapper<OrderSimpleVO> queryWrapper);

    /**
     * 商家、平台订单列表。包含买家、收货人、规格和支付信息。
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 管理端订单分页
     */
    @Select("select o.sn,o.flow_price,o.create_time,o.order_status,o.pay_status,o.payment_method,o.payment_time,o.member_name,o.member_id,o.consignee_name,o.consignee_mobile,o.store_name as " +
            "store_name,o.store_id as store_id,o.client_type,o.order_type,o.delivery_method,o.deliver_status,o.order_promotion_type,o.seller_remark " +
            ",GROUP_CONCAT(oi.goods_id) as group_goods_id," +
            " GROUP_CONCAT(oi.sku_id) as group_sku_id," +
            " GROUP_CONCAT(oi.num) as group_num" +
            ",GROUP_CONCAT(oi.image) as group_images" +
            ",GROUP_CONCAT(oi.goods_name) as group_name " +
            ",GROUP_CONCAT(IFNULL(oi.specs,'') SEPARATOR '\u001e') as group_specs " +
            ",GROUP_CONCAT(oi.after_sale_status) as group_after_sale_status" +
            ",GROUP_CONCAT(oi.complain_status) as group_complain_status" +
            ",GROUP_CONCAT(oi.comment_status) as group_comment_status" +
            ",GROUP_CONCAT(oi.sn) as group_order_items_sn " +
            ",GROUP_CONCAT(oi.goods_price) as group_goods_price " +
            ",GROUP_CONCAT(oi.is_refund) as group_is_refund " +
            ",GROUP_CONCAT(oi.refund_price) as group_refund_price " +
            " FROM li_order o LEFT JOIN li_order_item AS oi on o.sn = oi.order_sn ${ew.customSqlSegment} ")
    IPage<OrderSimpleVO> queryManageByParams(IPage<OrderSimpleVO> page, @Param(Constants.WRAPPER) Wrapper<OrderSimpleVO> queryWrapper);

    @Select("select COUNT(CASE WHEN order_status = 'UNPAID' THEN 1 END) as waitPayNum,"+
            "COUNT(CASE WHEN order_status = 'PAID' THEN 1 END) as waitDeliveryNum,"+
            "COUNT(CASE WHEN order_status = 'UNDELIVERED' THEN 1 END) as waitShipNum,"+
            "COUNT(CASE WHEN order_status = 'PARTS_DELIVERED' THEN 1 END) as partsDeliveredNumNum,"+
            "COUNT(CASE WHEN order_status = 'DELIVERED' AND order_type='NORMAL' THEN 1 END) as deliveredNum,"+
            "COUNT(CASE WHEN order_status = 'TAKE' THEN 1 END) as waitCheckNum,"+
            "COUNT(CASE WHEN order_status = 'STAY_PICKED_UP' THEN 1 END) as waitSelfPickNum,"+
            "COUNT(CASE WHEN order_status = 'COMPLETED' THEN 1 END) as finishNum,"+
            "COUNT(CASE WHEN order_status = 'CANCELLED' THEN 1 END) as closeNum "+
            " FROM li_order o ${ew.customSqlSegment} ")
    OrderNumVO getOrderNumVO(@Param(Constants.WRAPPER) Wrapper<OrderSimpleVO> queryWrapper);
    /**
     * 查询订单信息
     *
     * @param queryWrapper 查询条件
     * @return 简短订单分页
     */
    @Select("select o.* " +
            " FROM li_order o INNER JOIN li_order_item AS oi on o.sn = oi.order_sn ${ew.customSqlSegment} ")
    List<Order> queryListByParams(@Param(Constants.WRAPPER) Wrapper<Order> queryWrapper);

    /**
     * 卡密履约串行化：按订单号加行锁
     */
    @Select("SELECT * FROM li_order WHERE sn = #{orderSn} AND delete_flag = 0 FOR UPDATE")
    Order selectForUpdateBySn(@Param("orderSn") String orderSn);

    /**
     * 本店核销记录：预约核销码，以及虚拟/自提订单核销。
     * 商品名称、核销员、核销时间在外层过滤，避免 UNION 两侧各写一遍条件。
     *
     * @param page    分页
     * @param storeId 店铺ID
     * @param params  筛选条件，空字段不参与过滤
     * @return 核销记录
     */
    @Select("<script>" +
            "SELECT * FROM (" +
            "SELECT c.code AS code, c.order_sn AS order_sn, c.order_item_sn AS order_item_sn, " +
            "oi.goods_name AS goods_name, oi.image AS image, oi.specs AS specs, " +
            "c.verified_time AS verified_time, " +
            "IFNULL((SELECT cl.clerk_name FROM li_clerk cl WHERE cl.member_id = c.verified_by_user_id " +
            "AND cl.store_id = c.store_id AND cl.delete_flag = 0 LIMIT 1), " +
            "(SELECT mb.username FROM li_member mb WHERE mb.id = c.verified_by_user_id LIMIT 1)) AS verifier_name " +
            "FROM li_appointment_verification_code c " +
            "LEFT JOIN li_order_item oi ON oi.sn = c.order_item_sn AND oi.delete_flag = 0 " +
            "WHERE c.store_id = #{storeId} AND c.status = 'USED' AND c.delete_flag = 0 " +
            "UNION ALL " +
            "SELECT o.verification_code AS code, o.sn AS order_sn, oi.sn AS order_item_sn, " +
            "oi.goods_name AS goods_name, oi.image AS image, oi.specs AS specs, " +
            "IFNULL((SELECT ol.create_time FROM li_order_log ol WHERE ol.order_sn = o.sn AND ol.message LIKE '%核销%' " +
            "ORDER BY ol.create_time DESC LIMIT 1), o.update_time) AS verified_time, " +
            "(SELECT ol.operator_name FROM li_order_log ol WHERE ol.order_sn = o.sn AND ol.message LIKE '%核销%' " +
            "ORDER BY ol.create_time DESC LIMIT 1) AS verifier_name " +
            "FROM li_order o " +
            "INNER JOIN li_order_item oi ON oi.order_sn = o.sn AND oi.delete_flag = 0 " +
            "WHERE o.store_id = #{storeId} AND o.delete_flag = 0 " +
            "AND o.verification_code IS NOT NULL AND o.verification_code &lt;&gt; '' " +
            "AND o.order_status = 'COMPLETED' AND o.order_type &lt;&gt; 'APPOINTMENT' " +
            "AND (o.order_type = 'VIRTUAL' OR o.delivery_method = 'SELF_PICK_UP')" +
            ") verification_record " +
            "<where>" +
            "<if test='params.goodsName != null and params.goodsName != \"\"'> AND goods_name LIKE CONCAT('%', #{params.goodsName}, '%') </if>" +
            "<if test='params.verifierName != null and params.verifierName != \"\"'> AND verifier_name LIKE CONCAT('%', #{params.verifierName}, '%') </if>" +
            "<if test='params.startTime != null'> AND verified_time &gt;= #{params.startTime} </if>" +
            "<if test='params.endTime != null'> AND verified_time &lt;= #{params.endTime} </if>" +
            "</where>" +
            " ORDER BY verified_time DESC" +
            "</script>")
    IPage<OrderVerificationRecordVO> pageVerificationRecords(IPage<OrderVerificationRecordVO> page,
                                                             @Param("storeId") String storeId,
                                                             @Param("params") OrderVerificationRecordSearchParams params);

}