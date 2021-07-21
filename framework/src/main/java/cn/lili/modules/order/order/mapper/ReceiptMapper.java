package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderReceiptDTO;
import cn.lili.modules.order.order.entity.dto.ReceiptSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 发票数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 7:35 下午
 */
public interface ReceiptMapper extends BaseMapper<Receipt> {

    /**
     * 查询发票信息
     *
     * @param page              分页
     * @param queryWrapper 查询条件
     * @return
     */
    @Select("select r.*,o.order_status from li_receipt r inner join li_order o ON o.sn=r.order_sn ${ew.customSqlSegment}")
    IPage<OrderReceiptDTO> getReceipt(IPage<OrderSimpleVO> page, @Param(Constants.WRAPPER) Wrapper<ReceiptSearchParams> queryWrapper);
}