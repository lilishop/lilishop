package cn.lili.modules.live.util;

import cn.hutool.json.JSONObject;
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
import okhttp3.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.UnsupportedEncodingException;
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

    @Autowired
    private SettingService settingService;

    /**
     * 自定义推流地址
     *
     * @param liveRoom 直播活动
     * @return
     */
    public void getPushUrl(LiveRoom liveRoom) {

        long futureTimestamp = liveRoom.getStartTime().toInstant()
                .plus(30, ChronoUnit.DAYS)
                .getEpochSecond();
        LiveSetting liveSetting = getLiveSetting();
        liveRoom.setPushStreamServer("rtmp://" + liveSetting.getDomain() + "/" + liveSetting.getAppName() + "/");
        liveRoom.setPushStreamCode(liveSetting.getStreamName() + liveRoom.getSn() + getSafeUrl(liveSetting.getKey(), liveSetting.getStreamName() + liveRoom.getSn(), futureTimestamp));
//        return "rtmp://" + tencentLiveSetting.getDomain() + "/" + tencentLiveSetting.getAppName() + "/" + tencentLiveSetting.getStreamName() + liveRoom.getSn() + getSafeUrl(tencentLiveSetting.getKey(), tencentLiveSetting.getStreamName() + liveRoom.getSn(), futureTimestamp);
    }

    /**
     * 自定义拼接拉流地址
     *
     * @param liveRoom 直播活动
     * @return
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
        // 1. 生成一个随机的 int, 它的32位可以是任意组合
        ThreadLocalRandom random = ThreadLocalRandom.current();
        int randomIntBits = random.nextInt();
        // 2. 使用 Java 8 的方法将其转换为无符号的 long
        // 这个方法会正确处理负数（即最高位为1的情况）
        long unsignedInt = Integer.toUnsignedLong(randomIntBits);

        //设置群组参数
        Map<String, Object> contentTypeMap = new HashMap<>();
        contentTypeMap.put("Owner_Account", "administrator");
        contentTypeMap.put("Type", "AVChatRoom");
        //获取时间戳存储为字符串，设置为群组Id
        long timestampMillis = System.currentTimeMillis();
        String groupId = "20" + timestampMillis;
        contentTypeMap.put("GroupId", groupId);
        contentTypeMap.put("Name", "" + "ZSYK" + liveRoom.getId());
        //设置直播用户
        List<Map<String, Object>> memberList = new ArrayList<>();
        Map<String, Object> memberMap = new HashMap<>();
        memberMap.put("Member_Account", "administrator");
        memberMap.put("Role", "Admin");
//        memberList.add(memberMap);
//        contentTypeMap.put("MemberList", memberList);
        String contentTypeJson = JSONUtil.toJsonStr(contentTypeMap);

        try {
            // 3. 构造带查询参数的 URL
            HttpUrl.Builder urlBuilder = HttpUrl.parse("https://console.tim.qq.com/v4/group_open_http_svc/create_group").newBuilder();
            urlBuilder.addQueryParameter("sdkappid", liveSetting.getImSdkAppid() );
            urlBuilder.addQueryParameter("identifier", "administrator");
            urlBuilder.addQueryParameter("usersig", userSig);
            urlBuilder.addQueryParameter("random", String.valueOf(unsignedInt));
            urlBuilder.addQueryParameter("contenttype", "json");
            String url = urlBuilder.build().toString();

            RequestBody body = RequestBody.create(
                    MediaType.get("application/json; charset=utf-8"),
                    contentTypeJson
            );
            OkHttpClient client = new OkHttpClient();
            // 5. 构建并发送请求
            Request request = new Request.Builder().url(url).post(body).build();
            try (Response response = client.newCall(request).execute()) {
                System.out.println("Response Code: " + response.code());
                String respBodyStr = response.body().string();
                if (response.code() == 200) {
                    JSONObject respBody = JSONUtil.parseObj(respBodyStr);
                    liveRoom.setImGroupId(groupId);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void sendMessage(LiveMessage liveMessage) {
        LiveSetting liveSetting = getLiveSetting();
        String userSig = GenerateUserSig.genTestUserSig("administrator", liveSetting.getImSdkAppid(), liveSetting.getImSdkSecretKey());
        // 1. 生成一个随机的 int, 它的32位可以是任意组合
        ThreadLocalRandom random = ThreadLocalRandom.current();
        int randomIntBits = random.nextInt();
        // 2. 使用 Java 8 的方法将其转换为无符号的 long
        // 这个方法会正确处理负数（即最高位为1的情况）
        long unsignedInt = Integer.toUnsignedLong(randomIntBits);

        //设置群组参数
        Map<String, Object> contentTypeMap = new HashMap<>();
        int randomNum = ThreadLocalRandom.current().nextInt(1000000, 9999999 + 1);
        //随机数
        contentTypeMap.put("Random", randomNum);
        //群组Id
        contentTypeMap.put("GroupId", liveMessage.getImGroupId());
        //发送人
        contentTypeMap.put("From_Account", liveMessage.getUserId());
        //设置直播用户
        List<Map<String, Object>> msgBodyList = new ArrayList<>();
        //消息内容
        Map<String, Object> msgBodyMap = new HashMap<>();
        msgBodyMap.put("MsgType", "TIMTextElem");
        Map<String, String> msgContentMap = new HashMap<>();
        msgContentMap.put("Text", liveMessage.getMessage());
        msgBodyMap.put("MsgContent", msgContentMap);
        msgBodyList.add(msgBodyMap);
        contentTypeMap.put("MsgBody", msgBodyList);
        String contentTypeJson = JSONUtil.toJsonStr(contentTypeMap);
        try {
            // 3. 构造带查询参数的 URL
            HttpUrl.Builder urlBuilder = HttpUrl.parse("https://console.tim.qq.com/v4/group_open_http_svc/send_group_msg").newBuilder();
            urlBuilder.addQueryParameter("sdkappid", liveSetting.getImSdkAppid());
            urlBuilder.addQueryParameter("identifier", "administrator");
            urlBuilder.addQueryParameter("usersig", userSig);
            urlBuilder.addQueryParameter("random", String.valueOf(unsignedInt));
            urlBuilder.addQueryParameter("contenttype", "json");
            String url = urlBuilder.build().toString();

            RequestBody body = RequestBody.create(
                    MediaType.get("application/json; charset=utf-8"),
                    contentTypeJson
            );
            OkHttpClient client = new OkHttpClient();
            // 5. 构建并发送请求
            Request request = new Request.Builder().url(url).post(body).build();
            try (Response response = client.newCall(request).execute()) {
                log.info("发送消息," + liveMessage.getMessage() + "发送人：" + liveMessage.getUserName());
                String respBodyStr = response.body().string();
                log.info("发送结果:" + response);
                if (response.code() == 200) {
                    JSONObject respBody = JSONUtil.parseObj(respBodyStr);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void removeUserAllMessage(String userId, String groupId) {
        LiveSetting liveSetting = getLiveSetting();
        String userSig = GenerateUserSig.genTestUserSig("administrator", liveSetting.getImSdkAppid(), liveSetting.getImSdkSecretKey());
        // 1. 生成一个随机的 int, 它的32位可以是任意组合
        ThreadLocalRandom random = ThreadLocalRandom.current();
        int randomIntBits = random.nextInt();
        // 2. 使用 Java 8 的方法将其转换为无符号的 long
        // 这个方法会正确处理负数（即最高位为1的情况）
        long unsignedInt = Integer.toUnsignedLong(randomIntBits);

        //设置群组参数
        Map<String, Object> contentTypeMap = new HashMap<>();
        //群组Id
        contentTypeMap.put("GroupId", groupId);
        //发送人
        contentTypeMap.put("Sender_Account", userId.toString());
        String contentTypeJson = JSONUtil.toJsonStr(contentTypeMap);
        try {
            // 3. 构造带查询参数的 URL
            HttpUrl.Builder urlBuilder = HttpUrl.parse("https://console.tim.qq.com/v4/group_open_http_svc/delete_group_msg_by_sender").newBuilder();
            urlBuilder.addQueryParameter("sdkappid", liveSetting.getImSdkAppid());
            urlBuilder.addQueryParameter("identifier", "administrator");
            urlBuilder.addQueryParameter("usersig", userSig);
            urlBuilder.addQueryParameter("random", String.valueOf(unsignedInt));
            urlBuilder.addQueryParameter("contenttype", "json");
            String url = urlBuilder.build().toString();

            RequestBody body = RequestBody.create(
                    MediaType.get("application/json; charset=utf-8"),
                    contentTypeJson
            );
            OkHttpClient client = new OkHttpClient();
            // 5. 构建并发送请求
            Request request = new Request.Builder().url(url).post(body).build();
            try (Response response = client.newCall(request).execute()) {
                System.out.println("-------------------------撤回消息-----------------");
                System.out.println("发送人：" + userId + "群组" + groupId + "消息" + "撤回");
                System.out.println("Response Code: " + response.code());
                String respBodyStr = response.body().string();
                System.out.println("Response body: " + respBodyStr);
                if (response.code() == 200) {
                    JSONObject respBody = JSONUtil.parseObj(respBodyStr);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void forbidLiveStream(String liveStreamId) {
        LiveSetting liveSetting = getLiveSetting();
        try {
            // 实例化一个认证对象，入参需要传入腾讯云账户 SecretId 和 SecretKey，此处还需注意密钥对的保密
            // 代码泄露可能会导致 SecretId 和 SecretKey 泄露，并威胁账号下所有资源的安全性
            // 以下代码示例仅供参考，建议采用更安全的方式来使用密钥
            // 请参见：https://cloud.tencent.com/document/product/1278/85305
            // 密钥可前往官网控制台 https://console.cloud.tencent.com/cam/capi 进行获取
            Credential cred = createCredential();
            // 使用临时密钥示例
            // Credential cred = new Credential("SecretId", "SecretKey", "Token");
            // 实例化一个http选项，可选的，没有特殊需求可以跳过
            HttpProfile httpProfile = new HttpProfile();
            httpProfile.setEndpoint("live.tencentcloudapi.com");
            // 实例化一个client选项，可选的，没有特殊需求可以跳过
            ClientProfile clientProfile = new ClientProfile();
            clientProfile.setHttpProfile(httpProfile);
            // 实例化要请求产品的client对象,clientProfile是可选的
            LiveClient client = new LiveClient(cred, "", clientProfile);
            // 实例化一个请求对象,每个接口都会对应一个request对象
            ForbidLiveStreamRequest req = new ForbidLiveStreamRequest();
            req.setAppName(liveSetting.getAppName());
            req.setDomainName(liveSetting.getDomain());
            req.setStreamName(liveSetting.getStreamName() + liveStreamId);
            // 返回的resp是一个ForbidLiveStreamResponse的实例，与请求对象对应
            ForbidLiveStreamResponse resp = client.ForbidLiveStream(req);
            // 输出json格式的字符串回包
            System.out.println(AbstractModel.toJsonString(resp));
        } catch (TencentCloudSDKException e) {
            log.error("断开连接错误:{}", e);
        }
    }

    public void resumeLiveStream(String liveStreamId) {
        LiveSetting liveSetting = getLiveSetting();
        try {
            // 实例化一个认证对象，入参需要传入腾讯云账户 SecretId 和 SecretKey，此处还需注意密钥对的保密
            // 代码泄露可能会导致 SecretId 和 SecretKey 泄露，并威胁账号下所有资源的安全性
            // 以下代码示例仅供参考，建议采用更安全的方式来使用密钥
            // 请参见：https://cloud.tencent.com/document/product/1278/85305
            // 密钥可前往官网控制台 https://console.cloud.tencent.com/cam/capi 进行获取
            Credential cred = createCredential();
            // 使用临时密钥示例
            // Credential cred = new Credential("SecretId", "SecretKey", "Token");
            // 实例化一个http选项，可选的，没有特殊需求可以跳过
            HttpProfile httpProfile = new HttpProfile();
            httpProfile.setEndpoint("live.tencentcloudapi.com");
            // 实例化一个client选项，可选的，没有特殊需求可以跳过
            ClientProfile clientProfile = new ClientProfile();
            clientProfile.setHttpProfile(httpProfile);
            // 实例化要请求产品的client对象,clientProfile是可选的
            LiveClient client = new LiveClient(cred, "", clientProfile);
            // 实例化一个请求对象,每个接口都会对应一个request对象
            ResumeLiveStreamRequest req = new ResumeLiveStreamRequest();
            req.setAppName(liveSetting.getAppName());
            req.setDomainName(liveSetting.getDomain());
            req.setStreamName(liveSetting.getStreamName() + liveStreamId);
            // 返回的resp是一个ResumeLiveStreamResponse的实例，与请求对象对应
            ResumeLiveStreamResponse resp = client.ResumeLiveStream(req);
            // 输出json格式的字符串回包
            System.out.println(AbstractModel.toJsonString(resp));
        } catch (TencentCloudSDKException e) {
            log.error("恢复连接错误:{}", e);
        }
    }


    /**
     * 获取推流后缀
     * KEY+ streamName + txTime
     *
     * @param key        秘钥
     * @param streamName 流地址
     * @param txTime     过期时间
     * @return
     */
    private String getSafeUrl(String key, String streamName, long txTime) {
        String input = new StringBuilder().
                append(key).
                append(streamName).
                append(Long.toHexString(txTime).toUpperCase()).toString();

        String txSecret = null;
        try {
            MessageDigest messageDigest = MessageDigest.getInstance("MD5");
            txSecret = byteArrayToHexString(
                    messageDigest.digest(input.getBytes("UTF-8")));
        } catch (NoSuchAlgorithmException e) {
            e.printStackTrace();
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }

        return txSecret == null ? "" :
                new StringBuilder().
                        append("?txSecret=").
                        append(txSecret).
                        append("&").
                        append("txTime=").
                        append(Long.toHexString(txTime).toUpperCase()).
                        toString();
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
