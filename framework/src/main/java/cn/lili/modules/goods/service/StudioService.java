package cn.lili.modules.goods.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.Studio;
import cn.lili.modules.goods.entity.vos.StudioVO;
import cn.lili.trigger.message.BroadcastMessage;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 直播间业务层
 *
 * @author Bulbasaur
 * @since 2021/5/17 10:02 上午
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
     * 修改直播间
     * 直播间默认手机直播
     * @param studio 直播间
     * @return 修改状态
     */
    Boolean edit(Studio studio);

    /**
     * 获取直播间信息
     * @param id 直播间ID
     * @return 直播间VO
     */
    StudioVO getStudioVO(String id);

    /**
     * 获取直播间回放
     * @param roomId 房间ID
     * @return 直播间回放地址
     */
    String getLiveInfo(Integer roomId);

    /**
     * 推送商品
     * @param roomId 房间ID
     * @param goodsId 商品ID
     * @param storeId 店铺ID
     * @return 操作结果
     */
    Boolean push(Integer roomId,Integer goodsId, String storeId);

    /**
     * 删除商品
     * @param roomId 店铺ID
     * @param goodsId 商品ID
     * @return 操作结果
     */
    Boolean goodsDeleteInRoom(Integer roomId,Integer goodsId, String storeId);

    /**
     * 获取直播间列表
     * @param pageVO 分页
     * @param recommend 是否推荐
     * @param status 直播间状态
     * @return 直播间分页
     */
    IPage<StudioVO> studioList(PageVO pageVO, Integer recommend, String status);

    /**
     * 修改直播间状态
     * @param broadcastMessage 直播间消息
     */
    void updateStudioStatus(BroadcastMessage broadcastMessage);
}
