package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

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
}