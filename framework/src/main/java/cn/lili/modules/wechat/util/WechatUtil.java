package cn.lili.modules.wechat.util;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.crypto.digest.DigestUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.ConnectEnum;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.payment.kit.core.XmlHelper;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.WechatOfficialAccountSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.protocol.HTTP;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@Component
public class WechatUtil {

    private static final String CREATE_QR_CODE_API = "https://api.weixin.qq.com/cgi-bin/qrcode/create?access_token=";
    private static final String SHOW_QR_CODE_API = "https://mp.weixin.qq.com/cgi-bin/showqrcode?ticket=";
    private static final String USER_INFO_API = "https://api.weixin.qq.com/cgi-bin/user/info?access_token=";

    private static final String XML_MSG_TYPE = "/xml/MsgType";
    private static final String XML_FROM_USER = "/xml/FromUserName";
    private static final String XML_TO_USER = "/xml/ToUserName";
    private static final String XML_EVENT = "/xml/Event";
    private static final String XML_EVENT_KEY = "/xml/EventKey";

    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private MemberExperienceService memberExperienceService;
    @Autowired
    private SettingService settingService;

    /**
     * 校验微信公众号服务器接入签名
     */
    public boolean checkSignature(String signature, String timestamp, String nonce) {
        if (CharSequenceUtil.isBlank(signature) || CharSequenceUtil.isBlank(timestamp) || CharSequenceUtil.isBlank(nonce)) {
            return false;
        }
        WechatOfficialAccountSetting oaSetting = getOaSetting();
        String token = oaSetting == null ? null : oaSetting.getServerToken();
        if (CharSequenceUtil.isBlank(token)) {
            // 未配置 token 时默认放行（方便开发调试）
            return true;
        }
        String[] arr = new String[]{token, timestamp, nonce};
        Arrays.sort(arr);
        String combined = arr[0] + arr[1] + arr[2];
        String calculated = DigestUtil.sha1Hex(combined);
        return calculated.equalsIgnoreCase(signature);
    }

    public String h5QrCodeBase64(String scene, Integer expireSeconds) {
        String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.H5);
        if (CharSequenceUtil.isBlank(accessToken)) {
            throw new ServiceException(ResultCode.WECHAT_CONNECT_NOT_EXIST);
        }

        String sceneStr = CharSequenceUtil.isBlank(scene) ? "lilishop" : scene;
        int expire = expireSeconds == null ? 2592000 : Math.min(expireSeconds, 2592000);

        Map<String, Object> params = new HashMap<>(4);
        params.put("expire_seconds", expire);
        params.put("action_name", "QR_STR_SCENE");
        Map<String, Object> actionInfo = new HashMap<>(1);
        Map<String, Object> sceneMap = new HashMap<>(1);
        sceneMap.put("scene_str", sceneStr);
        actionInfo.put("scene", sceneMap);
        params.put("action_info", actionInfo);

        try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
            HttpPost httpPost = new HttpPost(CREATE_QR_CODE_API + accessToken);
            httpPost.addHeader(HTTP.CONTENT_TYPE, "application/json");
            String body = JSONUtil.toJsonStr(params);
            StringEntity entity = new StringEntity(body, StandardCharsets.UTF_8);
            entity.setContentType("application/json");
            httpPost.setEntity(entity);

            HttpResponse httpResponse = httpClient.execute(httpPost);
            HttpEntity httpEntity = httpResponse.getEntity();
            String content = new String(toByteArray(httpEntity.getContent()), StandardCharsets.UTF_8);
            JSONObject jsonObject = JSON.parseObject(content);
            Integer errcode = jsonObject.getInteger("errcode");
            if (errcode != null && errcode != 0) {
                log.error("微信公众号二维码创建失败:{}", content);
                throw new ServiceException(ResultCode.WECHAT_QRCODE_ERROR);
            }
            String ticket = jsonObject.getString("ticket");
            if (CharSequenceUtil.isBlank(ticket)) {
                log.error("微信公众号二维码创建返回缺少ticket:{}", content);
                throw new ServiceException(ResultCode.WECHAT_QRCODE_ERROR);
            }

            String showUrl = SHOW_QR_CODE_API + URLEncoder.encode(ticket, StandardCharsets.UTF_8.name());
            HttpGet httpGet = new HttpGet(showUrl);
            HttpResponse imageResponse = httpClient.execute(httpGet);
            HttpEntity imageEntity = imageResponse.getEntity();
            byte[] imageBytes = toByteArray(imageEntity.getContent());
            return Base64.getEncoder().encodeToString(imageBytes);
        } catch (ServiceException e) {
            throw e;
        } catch (Exception e) {
            throw new ServiceException(ResultCode.WECHAT_QRCODE_ERROR);
        }
    }

    /**
     * 处理微信公众号事件推送
     *
     * @param xml 微信发送的 XML 请求体
     * @return 返回被动回复的 XML，或者 "success"
     */
    public String handleH5Callback(String xml) {
        if (CharSequenceUtil.isBlank(xml)) {
            return "success";
        }
        XmlHelper xmlHelper = XmlHelper.of(xml);

        String msgType = xmlHelper.getString(XML_MSG_TYPE);
        String fromUser = xmlHelper.getString(XML_FROM_USER); // 粉丝 openId
        String toUser = xmlHelper.getString(XML_TO_USER);     // 公众号 gh_id
        String event = xmlHelper.getString(XML_EVENT);
        String eventKey = xmlHelper.getString(XML_EVENT_KEY);

        log.info("微信公众号回调 msgType={}, fromUser={}, toUser={}, event={}, eventKey={}", msgType, fromUser, toUser, event, eventKey);

        if (!"event".equalsIgnoreCase(msgType)) {
            return "success";
        }

        // 1. 取消关注事件：解绑公众号 openId
        if ("unsubscribe".equalsIgnoreCase(event)) {
            if (CharSequenceUtil.isNotBlank(fromUser)) {
                connectService.remove(new LambdaQueryWrapper<Connect>()
                        .eq(Connect::getUnionId, fromUser)
                        .eq(Connect::getUnionType, SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name()));
                log.info("微信公众号用户取消关注，已解除绑定 openId={}", fromUser);
            }
            return "success";
        }

        // 2. 关注 (subscribe) 或 扫码 (SCAN) 事件
        if (!"subscribe".equalsIgnoreCase(event) && !"SCAN".equalsIgnoreCase(event)) {
            return "success";
        }
        if (CharSequenceUtil.isBlank(fromUser)) {
            return "success";
        }

        try {
            String openIdUnionType = SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name();
            String unionIdUnionType = ConnectEnum.WECHAT.name();

            // 优先从微信拉取粉丝的 unionId
            String unionId = getWechatUnionId(fromUser);

            // 解析 memberId：
            // A. 带参数二维码方式（扫码后 eventKey 中有 scene_str/memberId）
            String memberId = null;
            if (CharSequenceUtil.isNotBlank(eventKey)) {
                String scene = eventKey.startsWith("qrscene_") ? eventKey.substring("qrscene_".length()) : eventKey;
                memberId = resolveMemberId(scene);
            }

            // B. 文章引导手动关注方式（没有带参数二维码，通过同一个微信开放平台的 unionId 匹配会员）
            if (CharSequenceUtil.isBlank(memberId) && CharSequenceUtil.isNotBlank(unionId)) {
                Connect unionConnect = connectService.queryConnect(
                        ConnectQueryDTO.builder()
                                .unionType(unionIdUnionType)
                                .unionId(unionId)
                                .build()
                );
                if (unionConnect != null) {
                    memberId = unionConnect.getUserId();
                    log.info("微信公众号关注通过 UnionId 自动匹配会员成功: unionId={}, memberId={}", unionId, memberId);
                } else {
                    log.warn("微信公众号关注未匹配到会员(该用户可能未在小程序/APP等使用微信登录): openId={}, unionId={}", fromUser, unionId);
                }
            }

            if (CharSequenceUtil.isNotBlank(memberId)) {
                Connect existedOpenId = connectService.queryConnect(
                        ConnectQueryDTO.builder().unionType(openIdUnionType).unionId(fromUser).build()
                );
                Connect existedUnionId = CharSequenceUtil.isBlank(unionId) ? null : connectService.queryConnect(
                        ConnectQueryDTO.builder().unionType(unionIdUnionType).unionId(unionId).build()
                );
                Connect existedMemberOpenId = connectService.queryConnect(
                        ConnectQueryDTO.builder().unionType(openIdUnionType).userId(memberId).build()
                );
                Connect existedMemberUnionId = connectService.queryConnect(
                        ConnectQueryDTO.builder().unionType(unionIdUnionType).userId(memberId).build()
                );

                if (existedOpenId != null && !memberId.equals(existedOpenId.getUserId())) {
                    log.warn("微信公众号回调绑定冲突: memberId={}, openId={} 已绑定到 userId={}", memberId, fromUser, existedOpenId.getUserId());
                    return buildTextReply(fromUser, toUser, "绑定失败：该微信已被其他账号绑定");
                }
                if (existedUnionId != null && !memberId.equals(existedUnionId.getUserId())) {
                    log.warn("微信公众号回调绑定冲突: memberId={}, unionId={} 已绑定到 userId={}", memberId, unionId, existedUnionId.getUserId());
                    return buildTextReply(fromUser, toUser, "绑定失败：该微信已被其他账号绑定");
                }

                boolean alreadyBound = existedOpenId != null || existedUnionId != null || existedMemberOpenId != null || existedMemberUnionId != null;

                if (existedOpenId == null) {
                    connectService.loginBindUser(memberId, fromUser, openIdUnionType);
                }
                if (CharSequenceUtil.isNotBlank(unionId) && existedUnionId == null) {
                    connectService.loginBindUser(memberId, unionId, unionIdUnionType);
                }

                if (!alreadyBound) {
                    memberExperienceService.grantExperience(memberId, ExperienceRuleEnum.BIND_WECHAT, fromUser, "绑定微信公众号");
                }
            }

            // 如果后台配置了关注自动回复文案，则被动回复粉丝
            WechatOfficialAccountSetting oaSetting = getOaSetting();
            String reply = oaSetting == null ? null : oaSetting.getSubscribeReply();
            if (CharSequenceUtil.isNotBlank(reply)) {
                return buildTextReply(fromUser, toUser, reply);
            }
        } catch (Exception e) {
            log.error("微信公众号关注绑定/自动回复处理失败 openId={}", fromUser, e);
        }

        return "success";
    }

    private String buildTextReply(String toUser, String fromUser, String content) {
        if (CharSequenceUtil.isBlank(content) || CharSequenceUtil.isBlank(toUser) || CharSequenceUtil.isBlank(fromUser)) {
            return "success";
        }
        long createTime = System.currentTimeMillis() / 1000;
        return "<xml>\n" +
                "  <ToUserName><![CDATA[" + toUser + "]]></ToUserName>\n" +
                "  <FromUserName><![CDATA[" + fromUser + "]]></FromUserName>\n" +
                "  <CreateTime>" + createTime + "</CreateTime>\n" +
                "  <MsgType><![CDATA[text]]></MsgType>\n" +
                "  <Content><![CDATA[" + content + "]]></Content>\n" +
                "</xml>";
    }

    private WechatOfficialAccountSetting getOaSetting() {
        try {
            Setting setting = settingService.get(SettingEnum.WECHAT_OFFICIAL_ACCOUNT_SETTING.name());
            if (setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())) {
                return JSONUtil.toBean(setting.getSettingValue(), WechatOfficialAccountSetting.class);
            }
        } catch (Exception e) {
            log.warn("获取公众号通知配置异常", e);
        }
        return null;
    }

    private String resolveMemberId(String scene) {
        return scene;
    }

    private String getWechatUnionId(String openId) {
        try {
            String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.H5);
            if (CharSequenceUtil.isBlank(accessToken)) {
                return null;
            }
            String url = USER_INFO_API + accessToken +
                    "&openid=" + URLEncoder.encode(openId, StandardCharsets.UTF_8.name()) +
                    "&lang=zh_CN";
            try (CloseableHttpClient httpClient = HttpClients.createDefault()) {
                HttpGet httpGet = new HttpGet(url);
                HttpResponse httpResponse = httpClient.execute(httpGet);
                HttpEntity httpEntity = httpResponse.getEntity();
                String content = new String(toByteArray(httpEntity.getContent()), StandardCharsets.UTF_8);
                JSONObject jsonObject = JSON.parseObject(content);
                Integer errcode = jsonObject.getInteger("errcode");
                if (errcode != null && errcode != 0) {
                    log.info("获取微信unionId失败 openId={}, resp={}", openId, content);
                    return null;
                }
                return jsonObject.getString("unionid");
            }
        } catch (Exception e) {
            log.error("获取微信unionId异常 openId={}", openId, e);
            return null;
        }
    }

    private static byte[] toByteArray(InputStream input) throws IOException {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        copy(input, output);
        return output.toByteArray();
    }

    private static int copy(InputStream input, OutputStream output) throws IOException {
        long count = copyLarge(input, output);
        if (count > Integer.MAX_VALUE) {
            return -1;
        }
        return (int) count;
    }

    private static long copyLarge(InputStream input, OutputStream output) throws IOException {
        byte[] buffer = new byte[4096];
        long count = 0L;
        int n;
        while (-1 != (n = input.read(buffer))) {
            output.write(buffer, 0, n);
            count += n;
        }
        return count;
    }
}
