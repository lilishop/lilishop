package cn.lili.modules.stock.mapper;

import cn.lili.modules.stock.entity.dos.StockOutboundItem;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * 商家出库单商品明细数据访问层
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Mapper
public interface StockOutboundItemMapper extends BaseMapper<StockOutboundItem> {
}
