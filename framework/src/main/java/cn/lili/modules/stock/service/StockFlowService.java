package cn.lili.modules.stock.service;

import cn.lili.modules.stock.entity.dos.StockFlow;
import cn.lili.modules.stock.entity.dto.StockFlowMessage;
import cn.lili.modules.stock.entity.params.StockFlowSearchParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 库存流水服务接口
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public interface StockFlowService extends IService<StockFlow> {

    /**
     * 分页查询库存流水记录
     *
     * @param params 查询参数
     * @return 分页结果
     */
    IPage<StockFlow> page(StockFlowSearchParams params);

    /**
     * 消费库存流水消息并持久化入库，通过唯一索引实现幂等去重
     *
     * @param message 库存流水消息体
     */
    void saveFlowFromMessage(StockFlowMessage message);
}
