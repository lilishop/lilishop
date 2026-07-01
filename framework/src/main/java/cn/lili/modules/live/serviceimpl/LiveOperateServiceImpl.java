package cn.lili.modules.live.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.live.entity.dos.LiveMessage;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.entity.dos.LiveUser;
import cn.lili.modules.live.entity.enums.LiveMessageAuthStatusEnum;
import cn.lili.modules.live.entity.enums.LiveStautsEnum;
import cn.lili.modules.live.mapper.LiveRoomMapper;
import cn.lili.modules.live.service.LiveMessageService;
import cn.lili.modules.live.service.LiveOperateService;
import cn.lili.modules.live.service.LiveRoomService;
import cn.lili.modules.live.service.LiveUserService;
import cn.lili.modules.live.util.GenerateUserSig;
import cn.lili.modules.live.util.MqttUtil;
import cn.lili.modules.live.util.TencentLiveUtil;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.LiveSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
@RequiredArgsConstructor
public class LiveOperateServiceImpl extends ServiceImpl<LiveRoomMapper, LiveRoom> implements LiveOperateService {

    @Autowired
    private Cache cache;

    @Autowired
    private TencentLiveUtil tencentLiveUtil;

    @Autowired
    private LiveUserService liveUserService;

    @Autowired
    private OrderService orderService;

    @Autowired
    private MqttUtil mqttUtil;

    @Autowired
    private LiveMessageService liveMessageService;

    @Autowired
    private LiveRoomService liveRoomService;

    @Autowired
    private SettingService settingService;

    private final String MESSAGE = "/message/";

    private Long LIVE_DETAIL_CACHE_TIME = 86400L;



    @Override
    public LiveUser checkLiveUser(String liveRoomId) {
        LiveRoom liveRoomUserDetail = liveRoomService.getLiveRoomUserDetail(liveRoomId);
        if(liveRoomUserDetail == null){
            throw new ServiceException(ResultCode.LIVE_USER_NOT_EXIST);
        }
        AuthUser currentUser = UserContext.getCurrentUser();
        if(currentUser == null){
            return null;
        }
        //用户被拉黑，提示已被拉黑
//        Blacklist blacklistByCache = blacklistService.getBlacklistByCache(user.getId(), liveActivity.getUserId());
//        if (blacklistByCache != null) {
//            throw new CrmebException(LiveActivityResultCode.USER_BLACKLIST_EXIST);
//        }
        LiveUser liveUser = liveUserService.queryLiveUserDetail(liveRoomId, currentUser.getId());
        if (liveUser == null) {
            this.increaseViewing(liveRoomId, currentUser.getId());
            LiveUser newLiveUser = new LiveUser();
            newLiveUser.setLiveRoomId(liveRoomId);
            newLiveUser.setUserId(currentUser.getId());
            newLiveUser.setUserName(currentUser.getNickName());
            newLiveUser.setUserFace(currentUser.getFace());
            newLiveUser.setUserSig(generateUserSig(currentUser.getId()));
            newLiveUser.setAmount(0D);
            newLiveUser.setWatchTime("00:00");
            newLiveUser.setMuteFlag(false);
            liveUser = liveUserService.getOrCreate(newLiveUser);
        }
        return liveUser;
    }

    @Override
    public void sendMessage(LiveMessage liveMessage) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if(currentUser == null){
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        //校验直播房间是否存在
        LiveRoom liveRoom = this.getById(liveMessage.getLiveRoomId());
        if(liveRoom == null){
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_EXIST);
        }
        //检测是否存在直播用户
        LiveUser liveUser = liveUserService.queryLiveUserDetail(liveMessage.getLiveRoomId(), currentUser.getId());
        if(liveUser == null){
            throw new ServiceException(ResultCode.LIVE_USER_NOT_EXIST);
        }
        if (liveUser.getMuteFlag()) {
            throw new ServiceException(ResultCode.LIVE_USER_MUTE_EXIST);
        }
        //校验直播房间是否为直播中
//        if (!liveRoom.getLiveStatus().equals(LiveStautsEnum.LIVING.name())) {
//            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_LIVING);
//        }
        liveMessage.setUserId(liveUser.getUserId());
        liveMessage.setUserName(liveUser.getUserName());
        liveMessage.setUserFace(liveUser.getUserFace());
        liveMessage.setImGroupId(liveRoom.getImGroupId());
        liveMessage.setAuthStatus(LiveMessageAuthStatusEnum.TOBEAUDITED.name());
        //校验用户是否有发送直播房间消息权限
        if(!liveRoom.getAuthFlag()){
            liveMessage.setAuthStatus(LiveMessageAuthStatusEnum.PASS.name());
            //发送消息
            tencentLiveUtil.sendMessage(liveMessage);
            //存入累计评论数量
            increaseCommentUser(liveUser.getLiveRoomId(), liveUser.getUserId());
            //发送消息数据
            sendMessageData(liveMessage);
        }
        liveMessageService.save(liveMessage);
    }

    @Override
    public void authMessage(LiveMessage liveMessage) {
        //校验直播房间是否存在
        LiveRoom liveRoom = this.getById(liveMessage.getLiveRoomId());
        if(liveRoom == null){
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_EXIST);
        }
        //校验直播房间是否为直播中
        if (!liveRoom.getLiveStatus().equals(LiveStautsEnum.LIVING.name())) {
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_LIVING);
        }
        if(liveMessage.getAuthStatus().equals(LiveMessageAuthStatusEnum.PASS.name())){
            liveMessage.setAuthStatus(LiveMessageAuthStatusEnum.PASS.name());
            //发送消息
            tencentLiveUtil.sendMessage(liveMessage);
            //存入累计评论数量
            increaseCommentUser(liveMessage.getLiveRoomId(), liveMessage.getUserId());
            //发送消息数据
            sendMessageData(liveMessage);
        }
        liveMessageService.updateById(liveMessage);
    }


    private void increaseCommentUser(String LiveRoomId, String userId) {
        String key = CachePrefix.LIVE_ACTIVITY_COMMENT_USER + LiveRoomId;
        cache.cumulative(key, userId);
        String commentKey = CachePrefix.LIVE_ACTIVITY_COMMENT + LiveRoomId;
        //检测是否存在key
        if (cache.hasKey(commentKey)) {
            //存在key，增加值
            cache.incr(commentKey,86400L);
        } else {
            //没有key，设置值为1
            cache.put(commentKey, 1, 86400L);
        }
    }

    private void sendMessageData(LiveMessage liveMessage) {
        String jsonStr = JSONUtil.toJsonStr(liveMessage);
        mqttUtil.sendSensorData(MESSAGE + liveMessage.getLiveRoomId(), jsonStr);
    }

    private String generateUserSig(String userId) {
        Setting setting = settingService.get(SettingEnum.LIVE_SETTING.name());
        LiveSetting liveSetting = JSONUtil.toBean(setting.getSettingValue(), LiveSetting.class);
        return GenerateUserSig.genTestUserSig(userId, liveSetting.getImSdkAppid(), liveSetting.getImSdkSecretKey());
    }   


    private void increaseViewing(String liveRoomId, String userId) {
        String key = CachePrefix.LIVE_ACTIVITY_VIEW.getPrefix() + liveRoomId;
        cache.cumulative(key, userId);
    }

    @Override
    public Long getCacheView(String liveRoomId){
        String key = CachePrefix.LIVE_ACTIVITY_VIEW.getPrefix() + liveRoomId;
        return cache.counter(key);
    }
}
