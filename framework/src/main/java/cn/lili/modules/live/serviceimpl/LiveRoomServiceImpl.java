package cn.lili.modules.live.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.live.entity.dto.LiveRoomSearchDTO;
import cn.lili.modules.live.entity.enums.LiveStautsEnum;
import cn.lili.modules.live.entity.vos.LiveRoomDetailVO;
import cn.lili.modules.live.entity.vos.LiveRoomVO;
import cn.lili.modules.live.mapper.LiveRoomMapper;
import cn.lili.modules.live.service.LiveRoomService;
import cn.lili.modules.live.service.LiveUserService;
import cn.lili.modules.live.util.MqttUtil;
import cn.lili.modules.live.util.TencentLiveUtil;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.ImSetting;
import cn.lili.modules.system.entity.dto.LiveSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
@RequiredArgsConstructor
public class LiveRoomServiceImpl extends ServiceImpl<LiveRoomMapper, LiveRoom> implements LiveRoomService {

    private final Cache cache;

    private final TencentLiveUtil tencentLiveUtil;

    private final LiveUserService liveUserService;

    private final OrderService orderService;

    private final Long LIVE_DETAIL_CACHE_TIME = 86400L;

    private final String LIVE_ROOM = "room/";

    private final MqttUtil mqttUtil;

    private final SettingService settingService;

    @Override
    public IPage<LiveRoom> queryPage(LiveRoomSearchDTO liveRoomSearchDTO) {
        return this.page(PageUtil.initPage(liveRoomSearchDTO), liveRoomSearchDTO.getQueryWrapper());
    }

    @Override
    public List<LiveRoom> queryList(LiveRoomSearchDTO liveRoomSearchDTO) {
        return this.list(liveRoomSearchDTO.getQueryWrapper());
    }

    @Override
    public boolean createLiveRoom(LiveRoom liveRoom) {
        liveRoom.setStoreId("-1");
        liveRoom.setStoreName("平台");
        liveRoom.setEndTime(null);

        liveRoom.setLiveStatus(LiveStautsEnum.NEW.name());
        //生成直播编码
        liveRoom.setSn(SnowFlake.createStr("L"));
        //获取推流地址
        tencentLiveUtil.getPushUrl(liveRoom);
        tencentLiveUtil.getPullUrl(liveRoom);
        return this.save(liveRoom);
    }

    @Override
    public boolean editLiveRoom(LiveRoom liveRoom) {
        boolean update = this.updateById(liveRoom);
        removeLiveRoomCache(liveRoom);
        return update;
    }

    @Override
    public void startLiveRoom(String  LiveRoomId) {
        LiveRoom liveRoom = this.getById(LiveRoomId);
        // 1. 校验直播流是否存在
        if (liveRoom == null) {
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_EXIST);
        }

        // 2. 校验直播流是否为新直播
        if (liveRoom.getLiveStatus().equals(LiveStautsEnum.LIVING.name())) {
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_LIVING);
        }
        //创建IM群组
        if(ObjectUtil.isNull(liveRoom.getImGroupId())){
            tencentLiveUtil.createImGroup(liveRoom);
        }
        // 6. 更新直播流状态为直播中
        liveRoom.setLiveStatus(LiveStautsEnum.LIVING.name());
        this.updateById(liveRoom);
        sendLiveData(liveRoom);
    }

    @Override
    public void endLiveRoom(String LiveRoomId) {
        LiveRoom liveRoom = this.getById(LiveRoomId);
        // 1. 校验直播流是否存在
        if (liveRoom == null) {
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_EXIST);
        }

        // 2. 校验直播流是否为直播中
        if (!liveRoom.getLiveStatus().equals(LiveStautsEnum.LIVING.name())) {
            throw new ServiceException(ResultCode.LIVE_ROOM_NOT_ENDED);
        }
        // 6. 更新直播流状态为已结束
        liveRoom.setLiveStatus(LiveStautsEnum.ENDED.name());
        liveRoom.setEndTime(new Date());

        //获取真实观看人数
        liveRoom.setActualViewNumber(liveUserService.queryLiveUserCount(LiveRoomId));
        //获取评论人数
        Object commentUserList = cache.get(CachePrefix.LIVE_ACTIVITY_COMMENT_USER + LiveRoomId);
        if (ObjectUtil.isNotNull(commentUserList)) {
            liveRoom.setCommentPeopleNumber(((List<Integer>) commentUserList).size());
        } else {
            liveRoom.setCommentPeopleNumber(0);
        }
        Object commentCount = cache.get(CachePrefix.LIVE_ACTIVITY_COMMENT + LiveRoomId);
        //获取评论数
        if (ObjectUtil.isNotNull(commentCount)) {
            liveRoom.setTotalComments((Integer) commentCount);
        } else {
            liveRoom.setTotalComments(0);
        }
        //直播结束切断推流
        tencentLiveUtil.forbidLiveStream(liveRoom.getId());
        this.updateById(liveRoom);
        sendLiveData(liveRoom);
    }

    @Override
    public LiveRoom getLiveRoomUserDetail(String id) {
        LiveRoom liveRoom = queryDetailByCache(id);
        //从缓存获取查看人数
        //根据人数随机生成观看数
        Long counter = getViewCount(id);
        liveRoom.setViewerCount((counter.intValue() * 3));
        //存入缓存
        setLiveCache(liveRoom);
        return liveRoom;
    }

    @Override
    public boolean editLiveMessageAuth(String id, Boolean authFlag) {
        LiveRoom liveRoom = this.getById(id);
        liveRoom.setAuthFlag(authFlag);
        setLiveCache(liveRoom);
        return this.updateById(liveRoom);
    }

    @Override
    public LiveRoomVO getLiveRoomDetailVO(String id) {
        LiveRoom liveRoom = this.getById(id);
        LiveRoomVO liveRoomVO = new LiveRoomVO();
        BeanUtil.copyProperties(liveRoom, liveRoomVO);
        //从缓存获取评论人数
        String commentUserKey = CachePrefix.LIVE_ACTIVITY_COMMENT_USER + id;
        Integer commentUserCount = cache.counter(commentUserKey).intValue();
        liveRoomVO.setCommentPeopleNumber(commentUserCount);

        //从缓存获取评论数量
        String commentKey = CachePrefix.LIVE_ACTIVITY_COMMENT + id;
        Integer commentCount = 0;
        Object commentCountObj = cache.get(commentKey);
        if (ObjectUtil.isNotNull(commentCountObj)) {
            commentCount = (Integer) commentCountObj;
        }
        liveRoomVO.setTotalComments(commentCount);

        //从缓存获取查看人数

        Double rate = 0D;
        Long counter = getViewCount(id);
        if (counter > 0) {
            rate = ((double) commentUserCount / counter.intValue()) * 100;
        }
        liveRoomVO.setActualViewNumber(Long.valueOf(counter.intValue()));

        liveRoomVO.setInteractionRate(rate);

        orderService.countLiveOrderData(liveRoomVO);


        Setting setting = settingService.get(SettingEnum.LIVE_SETTING.name());
        LiveSetting liveSetting = JSONUtil.toBean(setting.getSettingValue(), LiveSetting.class);
        liveRoomVO.setImSdkAppid(liveSetting.getImSdkAppid());
        liveRoomVO.setSecretId(liveSetting.getSecretId());
        return liveRoomVO;
    }

    @Override
    public Long getViewCount(String liveRoomId) {
        //从缓存获取查看人数
        String viewKey = CachePrefix.LIVE_ACTIVITY_VIEW + liveRoomId;
        return cache.counter(viewKey);
    }

    @Override
    public LiveRoomDetailVO queryDetail(String liveRoomId) {
        LiveRoom liveRoom = this.queryDetailByCache(liveRoomId);
        if(liveRoom == null){
            return null;
        }
        LiveRoomDetailVO detailVO = new LiveRoomDetailVO();
        BeanUtil.copyProperties(liveRoom, detailVO);

        Setting setting = settingService.get(SettingEnum.LIVE_SETTING.name());
        LiveSetting liveSetting = JSONUtil.toBean(setting.getSettingValue(), LiveSetting.class);
        detailVO.setImSdkAppid(liveSetting.getImSdkAppid());
        detailVO.setSecretId(liveSetting.getSecretId());
        return detailVO;
    }

    private void removeLiveRoomCache(LiveRoom liveRoom) {
        cache.remove(CachePrefix.LIVE_DETAIL.getPrefix() + liveRoom.getId());
    }

    private LiveRoom queryDetailByCache(String id) {
        Object liveRoomObj = cache.get(CachePrefix.LIVE_DETAIL + id);
        if (ObjectUtil.isNotNull(liveRoomObj)) {
            return (LiveRoom) liveRoomObj;
        }else{
            LiveRoom liveRoom = this.getById(id);
            setLiveCache(liveRoom);
            return liveRoom;
        }
    }

    private void setLiveCache(LiveRoom liveRoom) {
        cache.put(CachePrefix.LIVE_DETAIL + liveRoom.getId(), liveRoom, LIVE_DETAIL_CACHE_TIME);
    }

    private void sendLiveData(LiveRoom liveRoom){
        //清除缓存
        cache.remove(CachePrefix.LIVE_DETAIL + liveRoom.getId());
        //发送mqtt消息
        String jsonStr = JSONUtil.toJsonStr(liveRoom);
        mqttUtil.sendSensorData(LIVE_ROOM + liveRoom.getId(), jsonStr);
    }

}
