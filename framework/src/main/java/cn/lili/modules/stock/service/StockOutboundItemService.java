package cn.lili.modules.stock.service;

import cn.lili.modules.stock.entity.dos.StockOutboundItem;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 商家出库单明细服务接口
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public interface StockOutboundItemService extends IService<StockOutboundItem> {

    /**
     * 根据出库单ID查询所有明细项
     *
     * @param outboundId 出库单ID
     * @return 明细项列表
     */
    List<StockOutboundItem> listByOutboundId(String outboundId);

    /**
     * 根据出库单ID删除所有明细项（用于编辑覆盖重插）
     *
     * @param outboundId 出库单ID
     * @return 是否成功
     */
    boolean deleteByOutboundId(String outboundId);
}
