package cn.lili.modules.stock.serviceimpl;

import cn.lili.modules.stock.entity.dos.StockOutboundItem;
import cn.lili.modules.stock.mapper.StockOutboundItemMapper;
import cn.lili.modules.stock.service.StockOutboundItemService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;

/**
 * 商家出库单明细业务实现类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Service
public class StockOutboundItemServiceImpl extends ServiceImpl<StockOutboundItemMapper, StockOutboundItem> implements StockOutboundItemService {

    @Override
    public List<StockOutboundItem> listByOutboundId(String outboundId) {
        if (outboundId == null || outboundId.isBlank()) {
            return Collections.emptyList();
        }
        return this.list(new LambdaQueryWrapper<StockOutboundItem>()
                .eq(StockOutboundItem::getOutboundId, outboundId)
                .orderByAsc(StockOutboundItem::getId));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteByOutboundId(String outboundId) {
        if (outboundId == null || outboundId.isBlank()) {
            return false;
        }
        return this.remove(new LambdaQueryWrapper<StockOutboundItem>()
                .eq(StockOutboundItem::getOutboundId, outboundId));
    }
}
