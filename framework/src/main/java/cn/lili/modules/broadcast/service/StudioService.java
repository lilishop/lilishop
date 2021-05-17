package cn.lili.modules.broadcast.service;

import cn.lili.modules.broadcast.entity.dos.Studio;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 直播间业务层
 *
 * @author Bulbasaur
 * @date: 2021/5/17 10:02 上午
 */
public interface StudioService extends IService<Studio> {

    /**
     * 创建直播间
     * 直播间默认手机直播
     * 默认开启：点赞、商品货架、评论、回放
     * @param studio 直播间
     * @return 开启状态
     */
    Boolean create(Studio studio);

    /**
     * 获取直播间回放
     * @param roomId 房间ID
     * @return 直播间回放地址
     */
    String getLiveInfo(String roomId);

    /**
     * 推送商品
     * @param roomId 店铺ID
     * @param goodsId 商品ID
     * @return 操作结果
     */
    Boolean push(Integer roomId,Integer goodsId);

    /**
     * 删除商品
     * @param roomId 店铺ID
     * @param goodsId 商品ID
     * @return 操作结果
     */
    Boolean goodsDeleteInRoom(Integer roomId,Integer goodsId);
}
