package cn.lili.message;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.service.NoticeMessageService;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.SmsSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wechat.util.WechatSceneMessageSender;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.google.gson.Gson;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 会员消息编排器：按场景编码查询模板配置，统一分发站内信、短信、微信公众号、微信小程序四个渠道。
 * 各渠道相互隔离，单渠道异常只记日志；本类正常路径不抛异常，队列不会因此重试
 * （极端重投下站内信可能重复，当前接受该行为）。
 *
 * @author Bulbasaur
 * @since 2026-11-02
 */
@Slf4j
@Component
public class MemberMessageOrchestrator {

    @Autowired
    private NoticeMessageService noticeMessageService;
    @Autowired
    private SmsUtil smsUtil;
    @Autowired
    private WechatSceneMessageSender wechatSceneMessageSender;
    @Autowired
    private MemberService memberService;
    @Autowired
    private SettingService settingService;

    /**
     * 编排分发会员场景消息
     *
     * @param dto 场景消息（场景编码、会员ID、参数、小程序跳转路径）
     */
    public void orchestrate(NoticeMessageDTO dto) {
        if (dto == null || dto.getNoticeMessageNodeEnum() == null || CharSequenceUtil.isBlank(dto.getMemberId())) {
            return;
        }
        String sceneCode = dto.getNoticeMessageNodeEnum().name();
        try {
            NoticeMessage template = noticeMessageService.getOne(
                    new LambdaQueryWrapper<NoticeMessage>()
                            .eq(NoticeMessage::getType, "MEMBER")
                            .and(w -> w.eq(NoticeMessage::getSceneCode, sceneCode)
                                    .or()
                                    .eq(NoticeMessage::getNoticeNode, dto.getNoticeMessageNodeEnum().getDescription().trim()))
                            .last("LIMIT 1"), false);
            if (template == null) {
                log.info("消息模板不存在，跳过分发，scene={}", sceneCode);
                return;
            }
            // 站内信渠道（内部已做开关判断、变量替换与落库）
            try {
                noticeMessageService.noticeMessage(dto);
            } catch (Exception e) {
                log.error("站内信渠道发送失败，scene=" + sceneCode, e);
            }
            // 短信渠道
            try {
                sendSmsChannel(template, dto);
            } catch (Exception e) {
                log.error("短信渠道发送失败，scene=" + sceneCode, e);
            }
            // 微信公众号渠道
            try {
                wechatSceneMessageSender.sendOaMessage(sceneCode, dto.getMemberId(), dto.getParameter());
            } catch (Exception e) {
                log.error("微信公众号渠道发送失败，scene=" + sceneCode, e);
            }
            // 微信小程序订阅消息渠道
            try {
                wechatSceneMessageSender.sendMpMessage(sceneCode, dto.getMemberId(), dto.getParameter(), dto.getPage());
            } catch (Exception e) {
                log.error("微信小程序渠道发送失败，scene=" + sceneCode, e);
            }
        } catch (Exception e) {
            log.error("会员消息编排异常，scene=" + sceneCode, e);
        }
    }

    /**
     * 短信渠道：开关与模板 CODE 齐备、会员有手机号、系统短信签名已配置且非测试模式时发送
     */
    private void sendSmsChannel(NoticeMessage template, NoticeMessageDTO dto) {
        if (!SwitchEnum.OPEN.name().equalsIgnoreCase(template.getSmsStatus())
                || CharSequenceUtil.isBlank(template.getSmsTemplateCode())) {
            return;
        }
        Member member = memberService.getById(dto.getMemberId());
        if (member == null || CharSequenceUtil.isBlank(member.getMobile())) {
            log.info("会员无手机号，跳过短信，scene={}, memberId={}", template.getSceneCode(), dto.getMemberId());
            return;
        }
        Setting setting = settingService.get(SettingEnum.SMS_SETTING.name());
        SmsSetting smsSetting = setting != null && CharSequenceUtil.isNotBlank(setting.getSettingValue())
                ? new Gson().fromJson(setting.getSettingValue(), SmsSetting.class) : null;
        if (smsSetting == null || CharSequenceUtil.isBlank(smsSetting.getSignName())) {
            log.warn("短信默认签名未配置（SMS_SETTING.signName），跳过短信，scene={}", template.getSceneCode());
            return;
        }
        if (Boolean.TRUE.equals(smsSetting.getIsTestModel())) {
            log.info("短信测试模式，仅记录不发送：scene={}, mobile={}, templateCode={}",
                    template.getSceneCode(), member.getMobile(), template.getSmsTemplateCode());
            return;
        }
        smsUtil.sendNoticeSms(smsSetting.getSignName(), member.getMobile(), template.getSmsTemplateCode(), dto.getParameter());
        log.info("通知短信已发送：scene={}, mobile={}, templateCode={}", template.getSceneCode(), member.getMobile(), template.getSmsTemplateCode());
    }
}
