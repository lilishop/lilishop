package cn.lili.modules.order.order.mapper;

import cn.lili.modules.order.order.entity.dos.Trade;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Update;

/**
 * 交易数据处理层
 *
 * @author Chopper
 * @since 2020/11/17 7:35 下午
 */
public interface TradeMapper extends BaseMapper<Trade> {

    /**
     * 修改交易金额
     *
     * @param tradeSn 交易编号
     */
    @Update("UPDATE li_trade SET flow_price =(SELECT SUM(flow_price) FROM li_order WHERE trade_sn=#{tradeSn}) WHERE sn=#{tradeSn}")
    void updateTradePrice(String tradeSn);
}