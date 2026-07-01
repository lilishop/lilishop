package cn.lili.modules.live.util;

import cn.hutool.core.util.URLUtil;
import cn.hutool.http.HttpRequest;
import cn.hutool.http.HttpResponse;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.live.entity.dos.LiveMessage;
import cn.lili.modules.live.entity.dos.LiveRoom;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.LiveSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.tencentcloudapi.common.AbstractModel;
import com.tencentcloudapi.common.Credential;
import com.tencentcloudapi.common.exception.TencentCloudSDKException;
import com.tencentcloudapi.common.profile.ClientProfile;
import com.tencentcloudapi.common.profile.HttpProfile;
import com.tencentcloudapi.live.v20180801.LiveClient;
import com.tencentcloudapi.live.v20180801.models.ForbidLiveStreamRequest;
import com.tencentcloudapi.live.v20180801.models.ForbidLiveStreamResponse;
import com.tencentcloudapi.live.v20180801.models.ResumeLiveStreamRequest;
import com.tencentcloudapi.live.v20180801.models.ResumeLiveStreamResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

/**
 * 腾讯云直播工具类
 *
 * @author chc
 * @since 2022/6/2114:46
 */
@Slf4j
@Component
public class TencentLiveUtil {

    private static final char[] DIGITS_LOWER =
            {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f'};

    private static final String TIM_API_HOST = "https://console.tim.qq.com";

    @Autowired
    private SettingService settingService;

    /**
     * 自定义推流地址
     *
     * @param liveRoom 直播活动
     */
    public void getPushUrl(LiveRoom liveRoom) {
        long futureTimestamp = liveRoom.getStartTime().toInstant()
                .plus(30, ChronoUnit.DAYS)
                .getEpochSecond();
        LiveSetting liveSetting = getLiveSetting();
        liveRoom.setPushStreamServer("rtmp://" + liveSetting.getDomain() + "/" + liveSetting.getAppName() + "/");
        liveRoom.setPushStreamCode(liveSetting.getStreamName() + liveRoom.getSn() + getSafeUrl(liveSetting.getKey(), liveSetting.getStreamName() + liveRoom.getSn(), futureTimestamp));
    }

    /**
     * 自定义拼接拉流地址
     *
     * @param liveRoom 直播活动
     * @return 拉流地址
     */
    public String getPullUrlWithCodeRate(LiveRoom liveRoom, String template) {
        LiveSetting liveSetting = getLiveSetting();
        return "https://" + liveSetting.getPullDomain() + "/" + liveSetting.getAppName() + "/" + liveSetting.getStreamName() + liveRoom.getSn() + "_" + template + ".flv";
    }

    public void getPullUrl(LiveRoom liveRoom) {
        LiveSetting liveSetting = getLiveSetting();
        liveRoom.setPullStreamUrl("https://" + liveSetting.getPullDomain() + "/" + liveSetting.getAppName() + "/" + liveSetting.getStreamName() + liveRoom.getSn() + ".flv");
    }

    public void createImGroup(LiveRoom liveRoom) {
        LiveSetting liveSetting = getLiveSetting();
        String userSig = GenerateUserSig.genTestUserSig("administrator", liveSetting.getImSdkAppid(), liveSetting.getImSdkSecretKey());
        long unsignedInt = Integer.toUnsignedLong(ThreadLocalRandom.current().nextInt());

        Map<String, Object> contentTypeMap = new HashMap<>();
        contentTypeMap.put("Owner_Account", "administrator");
        contentTypeMap.put("Type", "AVChatRoom");
        long timestampMillis = System.currentTimeMillis();
        String groupId = "20" + timestampMillis;
        contentTypeMap.put("GroupId", groupId);
        contentTypeMap.put("Name", "ZSYK" + liveRoom.getId());
        String contentTypeJson = JSONUtil.toJsonStr(contentTypeMap);

        try {
            HttpResponse response = postTimRequest("/v4/group_open_http_svc/create_group",
                    liveSetting, userSig, unsignedInt, contentTypeJson);
            log.info("创建 IM 群组响应: status={}, body={}", response.getStatus(), response.body());
            if (response.isOk()) {
                liveRoom.setImGroupId(groupId);
            }
        } catch (Exception e) {
            log.error("创建 IM 群组失败", e);
        }
    }

    public void sendMessage(LiveMessage liveMessage) {
        LiveSetting liveSetting = getLiveSetting();
        String userSig = GenerateUserSig.genTestUserSig("administrator", liveSetting.getImSdkAppid(), liveSetting.getImSdkSecretKey());
        long unsignedInt = Integer.toUnsignedLong(ThreadLocalRandom.current().nextInt());

        Map<String, Object> contentTypeMap = new HashMap<>();
        contentTypeMap.put("Random", ThreadLocalRandom.current().nextInt(1000000, 10000000));
        contentTypeMap.put("GroupId", liveMessage.getImGroupId());
        contentTypeMap.put("From_Account", liveMessage.getUserId());

        List<Map<String, Object>> msgBodyList = new ArrayList<>();
        Map<String, Object> msgBodyMap = new HashMap<>();
        msgBodyMap.put("MsgType", "TIMTextElem");
        Map<String, String> msgContentMap = new HashMap<>();
        msgContentMap.put("Text", liveMessage.getMessage());
        msgBodyMap.put("MsgContent", msgContentMap);
        msgBodyList.add(msgBodyMap);
        contentTypeMap.put("MsgBody", msgBodyList);
        String contentTypeJson = JSONUtil.toJsonStr(contentTypeMap);

        try {
            HttpResponse response = postTimRequest("/v4/group_open_http_svc/send_group_msg",
                    liveSetting, userSig, unsignedInt, contentTypeJson);
            log.info("发送消息: {}, 发送人: {}, 响应: status={}, body={}",
                    liveMessage.getMessage(), liveMessage.getUserName(), response.getStatus(), response.body());
        } catch (Exception e) {
            log.error("发送 IM 消息失败", e);
        }
    }

    public void removeUserAllMessage(String userId, String groupId) {
        LiveSetting liveSetting = getLiveSetting();
        String userSig = GenerateUserSig.genTestUserSig("administrator", liveSetting.getImSdkAppid(), liveSetting.getImSdkSecretKey());
        long unsignedInt = Integer.toUnsignedLong(ThreadLocalRandom.current().nextInt());

        Map<String, Object> contentTypeMap = new HashMap<>();
        contentTypeMap.put("GroupId", groupId);
        contentTypeMap.put("Sender_Account", userId);
        String contentTypeJson = JSONUtil.toJsonStr(contentTypeMap);

        try {
            HttpResponse response = postTimRequest("/v4/group_open_http_svc/delete_group_msg_by_sender",
                    liveSetting, userSig, unsignedInt, contentTypeJson);
            log.info("撤回消息: userId={}, groupId={}, status={}, body={}",
                    userId, groupId, response.getStatus(), response.body());
        } catch (Exception e) {
            log.error("撤回 IM 消息失败", e);
        }
    }

    public void forbidLiveStream(String liveStreamId) {
        LiveSetting liveSetting = getLiveSetting();
        try {
            Credential cred = createCredential();
            HttpProfile httpProfile = new HttpProfile();
            httpProfile.setEndpoint("live.tencentcloudapi.com");
            ClientProfile clientProfile = new ClientProfile();
            clientProfile.setHttpProfile(httpProfile);
            LiveClient client = new LiveClient(cred, "", clientProfile);
            ForbidLiveStreamRequest req = new ForbidLiveStreamRequest();
            req.setAppName(liveSetting.getAppName());
            req.setDomainName(liveSetting.getDomain());
            req.setStreamName(liveSetting.getStreamName() + liveStreamId);
            ForbidLiveStreamResponse resp = client.ForbidLiveStream(req);
            log.info("禁推流结果: {}", AbstractModel.toJsonString(resp));
        } catch (TencentCloudSDKException e) {
            log.error("断开连接错误:{}", e.getMessage(), e);
        }
    }

    public void resumeLiveStream(String liveStreamId) {
        LiveSetting liveSetting = getLiveSetting();
        try {
            Credential cred = createCredential();
            HttpProfile httpProfile = new HttpProfile();
            httpProfile.setEndpoint("live.tencentcloudapi.com");
            ClientProfile clientProfile = new ClientProfile();
            clientProfile.setHttpProfile(httpProfile);
            LiveClient client = new LiveClient(cred, "", clientProfile);
            ResumeLiveStreamRequest req = new ResumeLiveStreamRequest();
            req.setAppName(liveSetting.getAppName());
            req.setDomainName(liveSetting.getDomain());
            req.setStreamName(liveSetting.getStreamName() + liveStreamId);
            ResumeLiveStreamResponse resp = client.ResumeLiveStream(req);
            log.info("恢复推流结果: {}", AbstractModel.toJsonString(resp));
        } catch (TencentCloudSDKException e) {
            log.error("恢复连接错误:{}", e.getMessage(), e);
        }
    }

    private HttpResponse postTimRequest(String path, LiveSetting liveSetting, String userSig,
                                          long random, String bodyJson) {
        String url = TIM_API_HOST + path
                + "?sdkappid=" + URLUtil.encodeAll(liveSetting.getImSdkAppid())
                + "&identifier=administrator"
                + "&usersig=" + URLUtil.encodeAll(userSig)
                + "&random=" + random
                + "&contenttype=json";
        return HttpRequest.post(url)
                .body(bodyJson)
                .contentType("application/json;charset=UTF-8")
                .timeout(10000)
                .execute();
    }

    private String getSafeUrl(String key, String streamName, long txTime) {
        String input = key + streamName + Long.toHexString(txTime).toUpperCase();
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("MD5");
            String txSecret = byteArrayToHexString(messageDigest.digest(input.getBytes(StandardCharsets.UTF_8)));
            return "?txSecret=" + txSecret + "&txTime=" + Long.toHexString(txTime).toUpperCase();
        } catch (NoSuchAlgorithmException e) {
            log.error("生成推流鉴权失败", e);
            return "";
        }
    }

    private Credential createCredential() {
        LiveSetting liveSetting = getLiveSetting();
        return new Credential(liveSetting.getSecretId(), liveSetting.getSecretKey());
    }

    private LiveSetting getLiveSetting() {
        Setting setting = settingService.get(SettingEnum.LIVE_SETTING.name());
        return JSONUtil.toBean(setting.getSettingValue(), LiveSetting.class);
    }

    private static String byteArrayToHexString(byte[] data) {
        char[] out = new char[data.length << 1];
        for (int i = 0, j = 0; i < data.length; i++) {
            out[j++] = DIGITS_LOWER[(0xF0 & data[i]) >>> 4];
            out[j++] = DIGITS_LOWER[0x0F & data[i]];
        }
        return new String(out);
    }
}
