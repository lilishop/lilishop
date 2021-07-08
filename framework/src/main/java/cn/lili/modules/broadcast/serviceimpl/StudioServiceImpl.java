package cn.lili.modules.broadcast.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONUtil;
import cn.lili.common.delayqueue.BroadcastMessage;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.trigger.enums.DelayTypeEnums;
import cn.lili.common.trigger.interfaces.TimeTrigger;
import cn.lili.common.trigger.model.TimeExecuteConstant;
import cn.lili.common.trigger.model.TimeTriggerMsg;
import cn.lili.common.trigger.util.DelayQueueTools;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.broadcast.entity.dos.Studio;
import cn.lili.modules.broadcast.entity.dos.StudioCommodity;
import cn.lili.modules.broadcast.entity.enums.StudioStatusEnum;
import cn.lili.modules.broadcast.entity.vos.StudioVO;
import cn.lili.modules.broadcast.mapper.CommodityMapper;
import cn.lili.modules.broadcast.mapper.StudioMapper;
import cn.lili.modules.broadcast.service.StudioCommodityService;
import cn.lili.modules.broadcast.service.StudioService;
import cn.lili.modules.broadcast.util.WechatLivePlayerUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.Map;

/**
 * 小程序直播间业务层实现
 *
 * @author Bulbasaur
 * @date: 2021/5/17 10:04 上午
 */
@Service
public class StudioServiceImpl extends ServiceImpl<StudioMapper, Studio> implements StudioService {

    @Autowired
    private WechatLivePlayerUtil wechatLivePlayerUtil;
    @Autowired
    private StudioCommodityService studioCommodityService;
    @Resource
    private CommodityMapper commodityMapper;
    @Autowired
    private TimeTrigger timeTrigger;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Override
    public Boolean create(Studio studio) {
    //创建小程序直播
    Map<String, String> roomMap = wechatLivePlayerUtil.create(studio);
    studio.setRoomId(Convert.toInt(roomMap.get("roomId")));
    studio.setQrCodeUrl(roomMap.get("qrcodeUrl"));
    studio.setStoreId(UserContext.getCurrentUser().getStoreId());
    studio.setStatus(StudioStatusEnum.NEW.name());
    //直播间添加成功发送直播间开启、关闭延时任务
    if (this.save(studio)) {
        //直播开启延时任务
        BroadcastMessage broadcastMessage = new BroadcastMessage(studio.getId(), StudioStatusEnum.START.name());
        TimeTriggerMsg timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.BROADCAST_EXECUTOR,
                Long.parseLong(studio.getStartTime()) * 1000L,
                broadcastMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.BROADCAST, studio.getId()),
                rocketmqCustomProperties.getPromotionTopic());

        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);

        //直播结束延时任务
        broadcastMessage = new BroadcastMessage(studio.getId(), StudioStatusEnum.END.name());
        timeTriggerMsg = new TimeTriggerMsg(TimeExecuteConstant.BROADCAST_EXECUTOR,
                Long.parseLong(studio.getEndTime()) * 1000L, broadcastMessage,
                DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.BROADCAST, studio.getId()),
                rocketmqCustomProperties.getPromotionTopic());
        //发送促销活动开始的延时任务
        this.timeTrigger.addDelay(timeTriggerMsg);
    }
    return true;

    }

    @Override
    public Boolean edit(Studio studio) {
        Studio oldStudio = this.getById(studio.getId());
        wechatLivePlayerUtil.editRoom(studio);
        if (this.updateById(studio)) {
            //发送更新延时任务
            //直播间开始
            BroadcastMessage broadcastMessage = new BroadcastMessage(studio.getId(), StudioStatusEnum.START.name());
            this.timeTrigger.edit(
                    TimeExecuteConstant.BROADCAST_EXECUTOR,
                    broadcastMessage,
                    Long.parseLong(oldStudio.getStartTime()) * 1000L,
                    Long.parseLong(studio.getStartTime()) * 1000L,
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.BROADCAST, studio.getId()),
                    DateUtil.getDelayTime(Long.parseLong(studio.getStartTime())),
                    rocketmqCustomProperties.getPromotionTopic());

            //直播间结束
            broadcastMessage = new BroadcastMessage(studio.getId(), StudioStatusEnum.START.name());
            this.timeTrigger.edit(
                    TimeExecuteConstant.BROADCAST_EXECUTOR,
                    broadcastMessage,
                    Long.parseLong(oldStudio.getEndTime()) * 1000L,
                    Long.parseLong(studio.getEndTime()) * 1000L,
                    DelayQueueTools.wrapperUniqueKey(DelayTypeEnums.BROADCAST, studio.getId()),
                    DateUtil.getDelayTime(Long.parseLong(studio.getEndTime())),
                    rocketmqCustomProperties.getPromotionTopic());
        }
        return true;
    }

    @Override
    public StudioVO getStudioVO(String id) {
        StudioVO studioVO = new StudioVO();
        //获取直播间信息
        BeanUtil.copyProperties(this.getById(id), studioVO);
        //获取直播间商品信息
        studioVO.setCommodityList(commodityMapper.getCommodityByRoomId(studioVO.getRoomId()));
        return studioVO;
    }

    @Override
    public String getLiveInfo(Integer roomId) {
        Studio studio = this.getByRoomId(roomId);
        //获取直播间并判断回放内容是否为空，如果为空则获取直播间回放并保存
        if (studio.getMediaUrl() != null) {
            return studio.getMediaUrl();
        } else {
            String mediaUrl = wechatLivePlayerUtil.getLiveInfo(roomId);
            studio.setMediaUrl(mediaUrl);
            this.save(studio);
            return mediaUrl;
        }
    }

    @Override
    public Boolean push(Integer roomId, Integer goodsId) {

        //判断直播间是否已添加商品
        if (studioCommodityService.getOne(
                new LambdaQueryWrapper<StudioCommodity>().eq(StudioCommodity::getRoomId, roomId)
                        .eq(StudioCommodity::getGoodsId, goodsId)) != null) {
            throw new ServiceException(ResultCode.STODIO_GOODS_EXIST_ERROR);
        }

        //调用微信接口添加直播间商品并进行记录
        if (wechatLivePlayerUtil.pushGoods(roomId, goodsId)) {
            studioCommodityService.save(new StudioCommodity(roomId, goodsId));
            //添加直播间商品数量
            Studio studio = this.getByRoomId(roomId);
            studio.setRoomGoodsNum(studio.getRoomGoodsNum() != null ? studio.getRoomGoodsNum() + 1 : 1);
            //设置直播间默认的商品（前台展示）只展示两个
            if (studio.getRoomGoodsNum() < 3) {
                studio.setRoomGoodsList(JSONUtil.toJsonStr(commodityMapper.getSimpleCommodityByRoomId(roomId)));
            }
            return this.updateById(studio);
        }
        return false;
    }

    @Override
    public Boolean goodsDeleteInRoom(Integer roomId, Integer goodsId) {
        //调用微信接口删除直播间商品并进行记录
        if (wechatLivePlayerUtil.goodsDeleteInRoom(roomId, goodsId)) {
            studioCommodityService.remove(new QueryWrapper<StudioCommodity>().eq("room_id", roomId).eq("goods_id", goodsId));
            //减少直播间商品数量
            Studio studio = this.getByRoomId(roomId);
            studio.setRoomGoodsNum(studio.getRoomGoodsNum() - 1);
            //设置直播间默认的商品（前台展示）只展示两个
            if (studio.getRoomGoodsNum() < 3) {
                studio.setRoomGoodsList(JSONUtil.toJsonStr(commodityMapper.getSimpleCommodityByRoomId(roomId)));
            }
            return this.updateById(studio);
        }
        return false;
    }

    @Override
    public IPage<Studio> studioList(PageVO pageVO, Integer recommend, String status) {
        QueryWrapper queryWrapper = new QueryWrapper<Studio>()
                .eq(recommend != null, "recommend", true)
                .eq(status != null, "status", status)
                .orderByDesc("create_time");
        if (UserContext.getCurrentUser() != null && UserContext.getCurrentUser().getRole().equals(UserEnums.STORE)) {
            queryWrapper.eq("store_id", UserContext.getCurrentUser().getStoreId());
        }
        return this.page(PageUtil.initPage(pageVO), queryWrapper);

    }

    @Override
    public void updateStudioStatus(BroadcastMessage broadcastMessage) {
        this.update(new LambdaUpdateWrapper<Studio>()
                .eq(Studio::getId, broadcastMessage.getStudioId())
                .set(Studio::getStatus, broadcastMessage.getStatus()));
    }

    /**
     * 根据直播间ID获取直播间
     *
     * @param roomId 直播间ID
     * @return 直播间
     */
    private Studio getByRoomId(Integer roomId) {
        return this.getOne(new LambdaQueryWrapper<Studio>().eq(Studio::getRoomId, roomId));
    }
}
