package cn.lili.modules.live.service;

import cn.lili.modules.live.entity.dos.LiveGoods;
import cn.lili.modules.order.order.entity.dos.Order;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
public interface LiveGoodsService extends IService<LiveGoods> {

    /**
     * 批量保存直播商品
     *
     * @param liveGoodsArray 直播商品列表
     */
    void saveBatchLiveGoodsArray(List<LiveGoods> liveGoodsArray);

    /**
     * 根据直播间ID获取直播商品列表
     *
     * @param liveId 直播间ID
     * @return 直播商品列表
     */
    List<LiveGoods> queryByLiveId(String liveId);

    /**
     * 根据直播间ID 从缓存获取直播商品列表
     *
     * @param liveId 直播间ID
     * @return 直播商品列表
     */
    List<LiveGoods> getByCache(String liveId);

    /**
     * 批量更新显示状态
     * @param ids 商品ID列表
     * @param show 是否显示
     * @return 是否成功
     */
    boolean batchUpdateShowStatus(List<String> ids, boolean show);

    /**
     * 设置推荐商品（同一时间只能有一个推荐商品）
     * @param id 商品ID
     * @return 是否成功
     */
    boolean setRecommend(String id);

    /**
     * 取消推荐商品
     * @param id 商品ID
     * @return 是否成功
     */
    boolean cancelRecommend(String id);

    /**
     * 根据订单获取直播商品列表，并增加购买数量
     *
     * @param order 订单对象
     * @return 直播商品列表
     */
    List<LiveGoods> getLiveGoodsByIdsAndAddNum(Order order);

    /**
     * 删除直播商品
     * @param ids 直播商品Ids
     * @return
     */
    boolean removeLiveGoods(List<String> ids);

    /**
     * 设置售罄状态
     * @param id
     * @param soldOutFlag
     * @return
     */
    boolean setSoldOut(String id, boolean soldOutFlag);

    /**
     * 增加商品热度
     * @param id
     */
    void addPopularity(String id, String liveId);

    /**
     * 设置商品热度
     * @param id
     * @param liveId
     * @param popularity
     */
    void setPopularity(String id, String liveId, Integer popularity);

    /**
     * 获取商品热度
     * @param id 商品Id
     * @param liveId 直播Id
     * @return
     */
    Long getPopularity(String id, String liveId);

    /**
     * 根据ID获取直播商品
     *
     * @param id 商品ID
     * @return 直播商品
     */
    List<LiveGoods> getLiveGoodsById(List<String> id) ;
}
