package cn.lili.modules.wechat.util;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.entity.enums.ConnectEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.member.entity.enums.ExperienceRuleEnum;
import cn.lili.modules.member.service.MemberExperienceService;
import cn.lili.modules.payment.kit.core.XmlHelper;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONObject;
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
    private static final String XML_EVENT = "/xml/Event";
    private static final String XML_EVENT_KEY = "/xml/EventKey";

    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private MemberExperienceService memberExperienceService;

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

    public void handleH5Callback(String xml) {
        if (CharSequenceUtil.isBlank(xml)) {
            return;
        }
        XmlHelper xmlHelper = XmlHelper.of(xml);

        String msgType = xmlHelper.getString(XML_MSG_TYPE);
        String fromUser = xmlHelper.getString(XML_FROM_USER);
        String event = xmlHelper.getString(XML_EVENT);
        String eventKey = xmlHelper.getString(XML_EVENT_KEY);

        log.info("微信公众号回调 msgType={}, fromUser={}, event={}, eventKey={}", msgType, fromUser, event, eventKey);

        if (!"event".equalsIgnoreCase(msgType)) {
            return;
        }
        if (!"subscribe".equalsIgnoreCase(event) && !"SCAN".equalsIgnoreCase(event)) {
            return;
        }
        if (CharSequenceUtil.isBlank(fromUser) || CharSequenceUtil.isBlank(eventKey)) {
            return;
        }

        String scene = eventKey;
        if (scene.startsWith("qrscene_")) {
            scene = scene.substring("qrscene_".length());
        }
        if (CharSequenceUtil.isBlank(scene)) {
            return;
        }

        String memberId = resolveMemberId(scene);
        if (CharSequenceUtil.isBlank(memberId)) {
            return;
        }

        try {
            String openIdUnionType = SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name();
            String unionIdUnionType = ConnectEnum.WECHAT.name();

            String unionId = getWechatUnionId(fromUser);

            cn.lili.modules.connect.entity.Connect existedOpenId = connectService.queryConnect(
                    ConnectQueryDTO.builder().unionType(openIdUnionType).unionId(fromUser).build()
            );
            cn.lili.modules.connect.entity.Connect existedUnionId = CharSequenceUtil.isBlank(unionId) ? null : connectService.queryConnect(
                    ConnectQueryDTO.builder().unionType(unionIdUnionType).unionId(unionId).build()
            );
            cn.lili.modules.connect.entity.Connect existedMemberOpenId = connectService.queryConnect(
                    ConnectQueryDTO.builder().unionType(openIdUnionType).userId(memberId).build()
            );
            cn.lili.modules.connect.entity.Connect existedMemberUnionId = connectService.queryConnect(
                    ConnectQueryDTO.builder().unionType(unionIdUnionType).userId(memberId).build()
            );

            if (existedOpenId != null && !memberId.equals(existedOpenId.getUserId())) {
                log.info("微信公众号回调绑定冲突: memberId={}, openId={} 已绑定到 userId={}", memberId, fromUser, existedOpenId.getUserId());
                return;
            }
            if (existedUnionId != null && !memberId.equals(existedUnionId.getUserId())) {
                log.info("微信公众号回调绑定冲突: memberId={}, unionId={} 已绑定到 userId={}", memberId, unionId, existedUnionId.getUserId());
                return;
            }

            boolean alreadyBound = existedOpenId != null || existedUnionId != null || existedMemberOpenId != null || existedMemberUnionId != null;

            if (existedOpenId == null) {
                connectService.loginBindUser(memberId, fromUser, openIdUnionType);
            }
            if (CharSequenceUtil.isNotBlank(unionId) && existedUnionId == null) {
                connectService.loginBindUser(memberId, unionId, unionIdUnionType);
            }

            if (alreadyBound) {
                log.info("微信公众号回调已绑定(不赠送经验值): memberId={}, openId={}, unionId={}, existedOpenIdUserId={}, existedUnionIdUserId={}",
                        memberId,
                        fromUser,
                        unionId,
                        existedOpenId == null ? null : existedOpenId.getUserId(),
                        existedUnionId == null ? null : existedUnionId.getUserId());
                return;
            }

            memberExperienceService.grantExperience(memberId, ExperienceRuleEnum.BIND_WECHAT, fromUser, "绑定微信公众号");
        } catch (Exception e) {
            log.error("微信公众号关注绑定/赠送经验值失败 memberId={}, openId={}", memberId, fromUser, e);
        }
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
