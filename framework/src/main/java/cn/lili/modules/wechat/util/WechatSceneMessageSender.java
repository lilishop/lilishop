package cn.lili.modules.wechat.util;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.http.HttpUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 场景化微信消息发送：按 sceneCode 查模板并推送，供消息编排器调用。
 * 关键字取值约定：模板关键字枚举名小写后作为参数 key（如 ORDER_SN → order_sn），缺失渲染为 "-"。
 *
 * @author Bulbasaur
 * @since 2026-11-02
 */
@Slf4j
@Component
public class WechatSceneMessageSender {

    @Autowired
    private WechatMPMessageService wechatMPMessageService;
    @Autowired
    private ConnectService connectService;
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    /**
     * 发送微信小程序订阅消息
     *
     * @param sceneCode 业务场景编码（对应 li_wechat_mp_message.scene_code）
     * @param memberId  会员ID
     * @param params    场景参数（key 为关键字枚举名小写）
     * @param page      小程序跳转路径，可空
     */
    public void sendMpMessage(String sceneCode, String memberId, Map<String, String> params, String page) {
        try {
            if (CharSequenceUtil.isBlank(sceneCode) || CharSequenceUtil.isBlank(memberId)) {
                return;
            }
            WechatMPMessage template = wechatMPMessageService.getOne(
                    new LambdaQueryWrapper<WechatMPMessage>().eq(WechatMPMessage::getSceneCode, sceneCode).last("LIMIT 1"), false);
            if (template == null || Boolean.FALSE.equals(template.getEnable()) || CharSequenceUtil.isBlank(template.getCode())) {
                log.info("小程序订阅消息未配置或未开启，scene={}", sceneCode);
                return;
            }
            Connect connect = connectService.queryConnect(
                    ConnectQueryDTO.builder().userId(memberId).unionType(SourceEnum.WECHAT_MP_OPEN_ID.name()).build());
            if (connect == null) {
                log.info("会员未绑定微信小程序，跳过订阅消息，scene={}, memberId={}", sceneCode, memberId);
                return;
            }
            String token = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);

            List<String> paramArray = JSON.parseArray(template.getKeywords(), String.class);
            List<String> texts = JSON.parseArray(template.getKeywordsText(), String.class);
            Map<String, String> data = new LinkedHashMap<>();
            for (int i = 0; i < paramArray.size(); i++) {
                String key = paramArray.get(i).toLowerCase();
                String val = params != null ? params.get(key) : null;
                if (CharSequenceUtil.isBlank(val)) {
                    val = "-";
                }
                int maxLength = ("order_sn".equals(key) || "after_sale_sn".equals(key)) ? 32 : 20;
                data.put(texts.get(i), StringUtils.subStringLength(val, maxLength));
            }
            WechatMessageData wechatMessageData = new WechatMessageData();
            wechatMessageData.setMpMessageData(data);

            Map<String, Object> map = new HashMap<>(4);
            map.put("touser", connect.getUnionId());
            map.put("template_id", template.getCode());
            map.put("data", wechatMessageData.createMPData());
            if (CharSequenceUtil.isNotBlank(page)) {
                map.put("page", page);
            }
            String content = postJson("https://api.weixin.qq.com/cgi-bin/message/subscribe/send?access_token=" + token, JSON.toJSONString(map));
            log.info("小程序订阅消息发送结果：scene={}, memberId={}, result={}", sceneCode, memberId, content);
        } catch (Exception e) {
            log.error("小程序订阅消息发送异常，scene=" + sceneCode + ", memberId=" + memberId, e);
        }
    }

    /**
     * HTTP POST JSON，收口便于测试替换
     */
    protected String postJson(String url, String body) {
        return HttpUtil.post(url, body);
    }
}
