package cn.lili.modules.order.order.mapper;

import cn.hutool.core.date.DateTime;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Update;

import java.util.List;

/**
 * 子订单数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 7:34 下午
 */
public interface OrderItemMapper extends BaseMapper<OrderItem> {

    /**
     * 获取等待操作订单子项目
     *
     * @param queryWrapper 查询条件
     * @return 订单子项列表
     */
    @Select("SELECT * FROM li_order_item AS oi INNER JOIN li_order AS o ON oi.order_sn=o.sn ${ew.customSqlSegment}")
    List<OrderItem> waitOperationOrderItem(@Param(Constants.WRAPPER) Wrapper<OrderSimpleVO> queryWrapper);

    /**
     * 检查售后表，根据售后表中的业务判定是否需要更新售后状态
     *
     * @param expiredTime 过期时间
     */
    @Update(" UPDATE li_order_item AS oi " +
            "INNER JOIN li_order AS o ON oi.order_sn = o.sn " +
            "INNER JOIN li_after_sale AS af ON af.order_item_sn = oi.sn " +
            "SET oi.after_sale_status = 'EXPIRED' " +
            "WHERE " +
            "oi.after_sale_status in ('ALREADY_APPLIED','PART_AFTER_SALE') " +
            "AND af.service_status in ('COMPLETE','REFUSE','SELLER_TERMINATION','BUYER_CANCEL') " +
            "AND o.complete_time <  #{expiredTime} ")
    void expiredAfterSaleStatusExecuteByAfterSale(DateTime expiredTime);
}