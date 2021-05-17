package cn.lili.modules.broadcast.service;

import cn.lili.modules.broadcast.entity.dos.Commodity;

/**
 * 直播商品业务层
 *
 * @author Bulbasaur
 * @date: 2021/5/17 10:39 上午
 */
public interface CommodityService {

    /**
     * 添加直播商品
     * @return 添加结果
     */
    boolean addCommodity(Commodity commodity);

    /**
     * 删除直播商品
     * @param goodsId 直播商品ID
     * @return 删除结果
     */
    boolean deleteCommodity(String goodsId);

    /**
     * 查询微信小程序直播商品审核状态
     */
    void getGoodsWarehouse();
}
