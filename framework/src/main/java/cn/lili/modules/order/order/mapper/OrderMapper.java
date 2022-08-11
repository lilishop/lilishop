package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderExportDTO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
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
    @Select("SELECT o.sn,o.create_time,o.member_name,o.consignee_name,o.consignee_mobile,o.consignee_address_path,o.consignee_detail," +
            "o.payment_method, o.logistics_name,o.freight_price,o.goods_price,o.discount_price,o.flow_price,oi.goods_name,oi.num," +
            "o.remark,o.order_status,o.pay_status,o.deliver_status,o.need_receipt,o.store_name FROM li_order o LEFT JOIN li_order_item oi " +
            "ON oi.order_sn=o.sn ${ew.customSqlSegment}")
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
     * 查询订单简短信息分页
     *
     * @param page         分页
     * @param queryWrapper 查询条件
     * @return 简短订单分页
     */
    @Select("select o.sn,o.flow_price,o.create_time,o.order_status,o.pay_status,o.payment_method,o.payment_time,o.member_name,o.store_name as store_name,o.store_id as store_id,o.client_type,o.order_type,o.deliver_status,o.order_promotion_type " +
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
            " FROM li_order o LEFT JOIN li_order_item AS oi on o.sn = oi.order_sn ${ew.customSqlSegment} ")
    IPage<OrderSimpleVO> queryByParams(IPage<OrderSimpleVO> page, @Param(Constants.WRAPPER) Wrapper<OrderSimpleVO> queryWrapper);

    /**
     * 查询订单信息
     *
     * @param queryWrapper 查询条件
     * @return 简短订单分页
     */
    @Select("select o.* " +
            " FROM li_order o INNER JOIN li_order_item AS oi on o.sn = oi.order_sn ${ew.customSqlSegment} ")
    List<Order> queryListByParams(@Param(Constants.WRAPPER) Wrapper<Order> queryWrapper);


}