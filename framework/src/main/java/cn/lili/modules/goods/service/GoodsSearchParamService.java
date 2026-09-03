package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dos.GoodsSearchParam;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 商品参数倒排（搜索用）
 *
 * @author Mike
 * @date 2026-09-03
 */
public interface GoodsSearchParamService extends IService<GoodsSearchParam> {

    /**
     * 同事务内按 goodsId 重建倒排；paramsJson 为 null 时调用方应跳过，不要传 null 进来。
     *
     * @param goodsId    商品ID
     * @param paramsJson 与 {@code li_goods.params} 一致的最终 JSON
     */
    void syncByGoodsId(String goodsId, String paramsJson);

    /**
     * 现读 li_goods.params 并重建单个商品，独立小事务，返回写入行数。
     *
     * @param goodsId 商品ID
     * @return 写入倒排表的行数
     */
    int rebuildByGoodsId(String goodsId);

    /**
     * 提交全量重建任务，立即返回当前状态；实际执行在 worker 线程。
     *
     * @return 当前重建状态
     */
    RebuildStatus submitRebuildAll();

    /**
     * 读取全量重建的当前状态（来自 Redis，跨实例一致）。
     *
     * @return 当前重建状态
     */
    RebuildStatus getRebuildStatus();
}
