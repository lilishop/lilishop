package cn.lili.modules.stock.mapper;

import cn.lili.modules.stock.entity.dos.StockFlow;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Mapper;

/**
 * 库存流水数据访问层
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Mapper
public interface StockFlowMapper extends BaseMapper<StockFlow> {
}
