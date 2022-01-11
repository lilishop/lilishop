package cn.lili.modules.promotion.service;

import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.vos.SeckillVO;

import java.util.List;

/**
 * 秒杀业务层
 *
 * @author Chopper
 * @since 2020/11/18 9:45 上午
 */
public interface SeckillService extends AbstractPromotionsService<Seckill> {

    /**
     * 预创建活动数量
     */
    Integer PRE_CREATION = 7;

    /**
     * 获取秒杀活动信息
     *
     * @param id 秒杀活动id
     * @return 秒杀活动信息
     */
    SeckillVO getSeckillDetail(String id);

    /**
     * 初始化秒杀活动，默认开启三十天的秒杀活动
     */
    void init();

    /**
     * 获取当前可参与的活动数量
     *
     * @return 可参与活动数量
     */
    long getApplyNum();

    /**
     * 更新秒杀活动的商品数量
     *
     * @param seckillId 秒杀活动ID
     */
    void updateSeckillGoodsNum(String seckillId);

    /**
     * 更新商品索引限时抢购信息
     *
     * @param seckill        限时抢购信息
     * @param seckillApplies 限时抢购商品列表
     */
    void updateEsGoodsSeckill(Seckill seckill, List<SeckillApply> seckillApplies);

    /**
     * 删除商品索引限时抢购信息
     *
     * @param seckill 限时抢购信息
     * @param skuIds  商品skuId列表
     */
    void deleteEsGoodsSeckill(Seckill seckill, List<String> skuIds);

    /**
     * 设置秒杀活动的每个参与活动商品的详细时间
     *
     * @param seckill      秒杀活动信息
     * @param seckillApply 申请参与秒杀活动的商品信息
     */
    void setSeckillApplyTime(Seckill seckill, SeckillApply seckillApply);
}