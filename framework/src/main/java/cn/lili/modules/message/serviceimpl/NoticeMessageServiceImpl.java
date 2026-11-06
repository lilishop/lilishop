package cn.lili.modules.message.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.MessageTemplateAggregateDTO;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum;
import cn.lili.modules.message.mapper.NoticeMessageTemplateMapper;
import cn.lili.modules.message.service.MemberMessageService;
import cn.lili.modules.message.service.NoticeMessageService;
import cn.lili.modules.message.util.NoticeMessageNodeResolver;
import cn.lili.modules.wechat.entity.dos.WechatMPMessage;
import cn.lili.modules.wechat.entity.dos.WechatMessage;
import cn.lili.modules.wechat.service.WechatMPMessageService;
import cn.lili.modules.wechat.service.WechatMessageService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 通知类消息模板业务层实现
 *
 * @author Bulbasaur
 * @since 2020/12/8 9:48
 */
@Service
public class NoticeMessageServiceImpl extends ServiceImpl<NoticeMessageTemplateMapper, NoticeMessage> implements NoticeMessageService {

    @Autowired
    private MemberMessageService memberMessageService;
    @Autowired
    private WechatMessageService wechatMessageService;
    @Autowired
    private WechatMPMessageService wechatMPMessageService;

    @Override
    public IPage<NoticeMessage> getMessageTemplate(PageVO pageVO, String type) {
        if ("STORE".equalsIgnoreCase(type)) {
            ensureStoreMessagesExist();
        } else if ("PLATFORM".equalsIgnoreCase(type)) {
            ensurePlatformMessagesExist();
        } else {
            ensureMemberDataUpgraded();
        }
        // 构建查询参数
        QueryWrapper<NoticeMessage> messageTemplateQueryWrapper = new QueryWrapper<>();
        // 消息模板类型 (MEMBER / STORE / PLATFORM)
        if (CharSequenceUtil.isNotBlank(type)) {
            messageTemplateQueryWrapper.eq("type", type);
        }
        messageTemplateQueryWrapper.orderByAsc("id");
        // 查询数据返回
        return this.page(PageUtil.initPage(pageVO), messageTemplateQueryWrapper);
    }

    private synchronized void ensureMemberDataUpgraded() {
        // 1. 自动清理退款/退货拆分的4条旧记录并合并为【售后审核通知】
        long legacyCount = this.count(new LambdaQueryWrapper<NoticeMessage>().in(NoticeMessage::getSceneCode, "RETURN_GOODS_PASS", "RETURN_MONEY_PASS", "RETURN_GOODS_REFUSE", "RETURN_MONEY_REFUSE"));
        if (legacyCount > 0) {
            this.remove(new LambdaQueryWrapper<NoticeMessage>().in(NoticeMessage::getSceneCode, "RETURN_GOODS_PASS", "RETURN_MONEY_PASS", "RETURN_GOODS_REFUSE", "RETURN_MONEY_REFUSE"));
        }
        long auditCount = this.count(new LambdaQueryWrapper<NoticeMessage>().eq(NoticeMessage::getSceneCode, "AFTER_SALE_AUDIT"));
        if (auditCount == 0) {
            NoticeMessage n = createNotice(
                    "售后审核通知",
                    "售后审核结果通知",
                    "您的售后申请已审核，审核结果：#{audit_result}，请进入售后中心查看详情。",
                    "AFTER_SALE_AUDIT",
                    "MEMBER",
                    "audit_result",
                    "【商城平台】尊敬的会员：您的售后申请已审核，请登录商城在售后中心查看审核结果。"
            );
            n.setId("9");
            this.save(n);
        }

        // 2. 自动将“提现申请驳回通知”修改为“提现申请审核通知”
        this.update(new LambdaUpdateWrapper<NoticeMessage>()
                .set(NoticeMessage::getNoticeNode, "提现申请审核通知")
                .set(NoticeMessage::getNoticeTitle, "提现申请审核通知")
                .set(NoticeMessage::getSmsContent, "【商城平台】尊敬的会员：您的余额提现申请已审核完成，请登录商城在个人中心查看详情。")
                .like(NoticeMessage::getNoticeNode, "提现申请驳回"));

        // 3. 自动移除“售后提交成功通知”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getSceneCode, "AFTER_SALE_CREATE_SUCCESS")
                .or()
                .like(NoticeMessage::getNoticeNode, "售后提交成功"));

        // 4. 自动移除“订单评价提醒”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getSceneCode, "ORDER_EVALUATION")
                .or()
                .like(NoticeMessage::getNoticeNode, "订单评价"));

        // 5. 自动移除“订单取消成功通知”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getSceneCode, "ORDER_CANCEL_SUCCESS")
                .or()
                .like(NoticeMessage::getNoticeNode, "订单取消"));

        // 6. 自动移除会员端“订单提交成功通知”与“订单支付成功通知”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getType, "MEMBER")
                .and(w -> w.in(NoticeMessage::getSceneCode, "ORDER_CREATE_SUCCESS", "ORDER_PAY_SUCCESS")
                        .or()
                        .in(NoticeMessage::getNoticeNode, "订单提交成功通知", "订单支付成功通知")));

        // 7. 自动将退货物品签收/拒收通知合并为【退货物品收货通知】
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .in(NoticeMessage::getSceneCode, "AFTER_SALE_ROG", "AFTER_SALE_ROG_PASS", "AFTER_SALE_ROG_REFUSE")
                .or()
                .in(NoticeMessage::getNoticeNode, "退货物品收货通知", "退货物品签收通知", "退货物品拒收通知"));

        // 8. 自动移除“售后完成通知”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getSceneCode, "AFTER_SALE_COMPLETE")
                .or()
                .like(NoticeMessage::getNoticeNode, "售后完成"));

        // 9. 自动移除“提现申请提交成功通知”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getSceneCode, "WALLET_WITHDRAWAL_CREATE")
                .or()
                .like(NoticeMessage::getNoticeNode, "提现申请提交成功")
                .or()
                .like(NoticeMessage::getNoticeNode, "余额提现申请提交成功"));

        // 10. 自动移除“余额提现成功通知”与“微信提现成功通知”
        this.remove(new LambdaQueryWrapper<NoticeMessage>()
                .eq(NoticeMessage::getType, "MEMBER")
                .and(w -> w.in(NoticeMessage::getNoticeNode, "余额提现成功通知", "微信提现成功通知")
                        .or()
                        .in(NoticeMessage::getSceneCode, "WALLET_WITHDRAWAL_SUCCESS")
                        .or()
                        .like(NoticeMessage::getNoticeNode, "提现成功")));

    }

    private synchronized void ensureStoreMessagesExist() {
        // 清理旧的退款/退货拆分数据
        this.remove(new LambdaQueryWrapper<NoticeMessage>().in(NoticeMessage::getSceneCode, "STORE_REFUND_APPLY", "STORE_RETURN_GOODS_APPLY"));

        long count = this.count(new LambdaQueryWrapper<NoticeMessage>().eq(NoticeMessage::getType, "STORE"));
        if (count == 0) {
            List<NoticeMessage> list = new ArrayList<>();
            list.add(createNotice("店铺入驻结果", "店铺入驻结果通知", "恭喜您，您的店铺入驻申请已经审核通过，请登录商家后台进行店铺管理。", "STORE_AUDIT_RESULT", "STORE", "store_name", "【商城平台】尊敬的商家：您的店铺入驻申请审核结果已出，请登录商家后台查看。"));
            list.add(createNotice("订单付款成功通知", "商户订单付款提醒", "您有新的订单付款成功，订单号：#{order_sn}，请尽快安排发货。", "STORE_ORDER_PAY_SUCCESS", "STORE", "order_sn", "【商城平台】尊敬的商家：您的店铺有新订单付款成功（单号：${order_sn}），请尽快安排发货。"));
            list.add(createNotice("用户售后申请通知", "用户售后申请提醒", "买家对订单 #{order_sn} 发起了售后申请，请及时在商家后台进行审核处理。", "STORE_AFTER_SALE_APPLY", "STORE", "order_sn", "【商城平台】尊敬的商家：买家已对订单 ${order_sn} 发起售后申请，请登录商家后台及时处理。"));
            this.saveBatch(list);
        } else {
            long afterSaleCount = this.count(new LambdaQueryWrapper<NoticeMessage>().eq(NoticeMessage::getSceneCode, "STORE_AFTER_SALE_APPLY"));
            if (afterSaleCount == 0) {
                this.save(createNotice("用户售后申请通知", "用户售后申请提醒", "买家对订单 #{order_sn} 发起了售后申请，请及时在商家后台进行审核处理。", "STORE_AFTER_SALE_APPLY", "STORE", "order_sn", "【商城平台】尊敬的商家：买家已对订单 ${order_sn} 发起售后申请，请登录商家后台及时处理。"));
            }
        }
    }

    private synchronized void ensurePlatformMessagesExist() {
        long count = this.count(new LambdaQueryWrapper<NoticeMessage>().eq(NoticeMessage::getType, "PLATFORM"));
        if (count == 0) {
            List<NoticeMessage> list = new ArrayList<>();
            NoticeMessage n = createNotice("新店铺入驻待审核通知", "新店铺入驻审核提醒", "", "PLATFORM_STORE_AUDIT_NOTIFY", "PLATFORM", "store_name", "【商城平台】尊敬的管理员：有新的店铺申请入驻（店铺名称：${store_name}），请及时登录管理后台审核。");
            n.setNoticeStatus(SwitchEnum.CLOSE.name());
            list.add(n);
            this.saveBatch(list);
        } else {
            // 平台端没有站内信，统一关闭
            this.update(new LambdaUpdateWrapper<NoticeMessage>()
                    .set(NoticeMessage::getNoticeStatus, SwitchEnum.CLOSE.name())
                    .eq(NoticeMessage::getType, "PLATFORM"));
        }
    }

    private NoticeMessage createNotice(String node, String title, String content, String sceneCode, String type, String variable, String smsContent) {
        NoticeMessage n = new NoticeMessage();
        n.setNoticeNode(node);
        n.setNoticeTitle(title);
        n.setNoticeContent(content);
        n.setNoticeStatus(SwitchEnum.OPEN.name());
        n.setSceneCode(sceneCode);
        n.setType(type);
        n.setVariable(variable);
        n.setSmsStatus(SwitchEnum.CLOSE.name());
        n.setSmsContent(smsContent);
        return n;
    }

    @Override
    public void noticeMessage(NoticeMessageDTO noticeMessageDTO) {
        if (noticeMessageDTO == null) {
            return;
        }
        try {
            NoticeMessage noticeMessage = this.getOne(
                    new LambdaQueryWrapper<NoticeMessage>()
                            .eq(NoticeMessage::getSceneCode, noticeMessageDTO.getNoticeMessageNodeEnum().name())
                            .or()
                            .eq(NoticeMessage::getNoticeNode, noticeMessageDTO.getNoticeMessageNodeEnum().getDescription().trim())
                            .last("LIMIT 1"), false);
            // 如果通知类站内信开启的情况下
            if (noticeMessage != null && SwitchEnum.OPEN.name().equalsIgnoreCase(noticeMessage.getNoticeStatus())) {
                MemberMessage memberMessage = new MemberMessage();
                memberMessage.setMemberId(noticeMessageDTO.getMemberId());
                memberMessage.setTitle(noticeMessage.getNoticeTitle());
                memberMessage.setContent(noticeMessage.getNoticeContent());
                // 参数不为空，替换内容
                if (noticeMessageDTO.getParameter() != null) {
                    memberMessage.setContent(replaceNoticeContent(noticeMessage.getNoticeContent(), noticeMessageDTO.getParameter()));
                } else {
                    memberMessage.setContent(noticeMessage.getNoticeContent());
                }
                memberMessage.setStatus(MessageStatusEnum.UN_READY.name());
                // 添加站内信
                memberMessageService.save(memberMessage);
            }
        } catch (Exception e) {
            log.error("站内信发送失败：", e);
        }
    }

    /**
     * 替换站内信内容
     *
     * @param noticeContent 站内信内容
     * @param parameter     参数
     * @return 替换后站内信内容
     */
    String replaceNoticeContent(String noticeContent, Map<String, String> parameter) {
        for (Map.Entry<String, String> entry : parameter.entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue() != null ? entry.getValue() : "";
            if (key != null) {
                noticeContent = noticeContent.replace("#{" + key.trim() + "}", value);
                noticeContent = noticeContent.replace("{" + key.trim() + "}", value);
            }
            String description = NoticeMessageParameterEnum.getValueByType(entry.getKey());
            if (description != null && entry.getValue() != null) {
                noticeContent = noticeContent.replace("#{" + description + "}".trim(), entry.getValue());
                noticeContent = noticeContent.replace("{" + description + "}".trim(), entry.getValue());
            }
            if (description != null && entry.getValue() != null) {
                noticeContent = noticeContent.replace("#{" + description + "}".trim(), entry.getValue());
            }
        }
        return noticeContent;
    }

    @Override
    public IPage<MessageTemplateAggregateDTO> getMessageTemplateAggregatePage(PageVO pageVO, String type) {
        IPage<NoticeMessage> noticePage = this.getMessageTemplate(pageVO, type);
        List<MessageTemplateAggregateDTO> rows = new ArrayList<>(noticePage.getRecords().size());
        for (NoticeMessage notice : noticePage.getRecords()) {
            MessageTemplateAggregateDTO dto = new MessageTemplateAggregateDTO();
            dto.setNotice(notice);
            String scene = resolveSceneCode(notice);
            if (CharSequenceUtil.isNotBlank(scene)) {
                dto.setWechatOa(wechatMessageService.getOne(
                        new LambdaQueryWrapper<WechatMessage>().eq(WechatMessage::getSceneCode, scene).last("LIMIT 1"),
                        false));
                dto.setWechatMp(wechatMPMessageService.getOne(
                        new LambdaQueryWrapper<WechatMPMessage>().eq(WechatMPMessage::getSceneCode, scene).last("LIMIT 1"),
                        false));
            }
            rows.add(dto);
        }
        Page<MessageTemplateAggregateDTO> out = new Page<>(noticePage.getCurrent(), noticePage.getSize(), noticePage.getTotal());
        out.setRecords(rows);
        return out;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateChannelSwitch(String id, String channel, String status) {
        NoticeMessage notice = this.getById(id);
        if (notice == null) {
            return false;
        }
        String normStatus = SwitchEnum.OPEN.name().equalsIgnoreCase(status) ? SwitchEnum.OPEN.name() : SwitchEnum.CLOSE.name();
        boolean isOpen = SwitchEnum.OPEN.name().equalsIgnoreCase(normStatus);

        if ("NOTICE".equalsIgnoreCase(channel)) {
            notice.setNoticeStatus(normStatus);
            return this.updateById(notice);
        } else if ("SMS".equalsIgnoreCase(channel)) {
            notice.setSmsStatus(normStatus);
            return this.updateById(notice);
        } else if ("WECHAT_OA".equalsIgnoreCase(channel)) {
            String scene = resolveSceneCode(notice);
            if (CharSequenceUtil.isBlank(scene)) return false;
            WechatMessage oa = wechatMessageService.getOne(
                    new LambdaQueryWrapper<WechatMessage>().eq(WechatMessage::getSceneCode, scene).last("LIMIT 1"), false);
            if (oa != null) {
                oa.setEnable(isOpen);
                return wechatMessageService.updateById(oa);
            } else {
                oa = new WechatMessage();
                oa.setSceneCode(scene);
                oa.setName(notice.getNoticeTitle());
                oa.setEnable(isOpen);
                return wechatMessageService.save(oa);
            }
        } else if ("WECHAT_MP".equalsIgnoreCase(channel)) {
            String scene = resolveSceneCode(notice);
            if (CharSequenceUtil.isBlank(scene)) return false;
            WechatMPMessage mp = wechatMPMessageService.getOne(
                    new LambdaQueryWrapper<WechatMPMessage>().eq(WechatMPMessage::getSceneCode, scene).last("LIMIT 1"), false);
            if (mp != null) {
                mp.setEnable(isOpen);
                return wechatMPMessageService.updateById(mp);
            } else {
                mp = new WechatMPMessage();
                mp.setSceneCode(scene);
                mp.setName(notice.getNoticeTitle());
                mp.setCode(null);
                mp.setEnable(isOpen);
                return wechatMPMessageService.save(mp);
            }
        }
        return false;
    }

    @Override
    public boolean updateNoticeTemplate(String id, String title, String content, String status) {
        NoticeMessage notice = this.getById(id);
        if (notice == null) return false;
        if (CharSequenceUtil.isNotBlank(title)) notice.setNoticeTitle(title);
        if (CharSequenceUtil.isNotBlank(content)) notice.setNoticeContent(content);
        if (CharSequenceUtil.isNotBlank(status)) {
            notice.setNoticeStatus(SwitchEnum.OPEN.name().equalsIgnoreCase(status) ? SwitchEnum.OPEN.name() : SwitchEnum.CLOSE.name());
        }
        return this.updateById(notice);
    }

    @Override
    public boolean updateSmsTemplate(String id, String smsTemplateCode, String status) {
        NoticeMessage notice = this.getById(id);
        if (notice == null) return false;
        notice.setSmsTemplateCode(smsTemplateCode);
        if (CharSequenceUtil.isNotBlank(status)) {
            notice.setSmsStatus(SwitchEnum.OPEN.name().equalsIgnoreCase(status) ? SwitchEnum.OPEN.name() : SwitchEnum.CLOSE.name());
        }
        return this.updateById(notice);
    }

    @Override
    public boolean updateWechatOaTemplate(String id, String code, Boolean enable) {
        NoticeMessage notice = this.getById(id);
        if (notice == null) return false;
        String scene = resolveSceneCode(notice);
        if (CharSequenceUtil.isBlank(scene)) return false;

        WechatMessage oa = wechatMessageService.getOne(
                new LambdaQueryWrapper<WechatMessage>().eq(WechatMessage::getSceneCode, scene).last("LIMIT 1"), false);
        if (oa != null) {
            oa.setCode(code);
            if (enable != null) oa.setEnable(enable);
            return wechatMessageService.updateById(oa);
        } else {
            oa = new WechatMessage();
            oa.setSceneCode(scene);
            oa.setName(notice.getNoticeTitle());
            oa.setCode(code);
            oa.setEnable(enable != null ? enable : true);
            return wechatMessageService.save(oa);
        }
    }

    @Override
    public boolean updateWechatMpTemplate(String id, String code, Boolean enable) {
        NoticeMessage notice = this.getById(id);
        if (notice == null) return false;
        String scene = resolveSceneCode(notice);
        if (CharSequenceUtil.isBlank(scene)) return false;

        WechatMPMessage mp = wechatMPMessageService.getOne(
                new LambdaQueryWrapper<WechatMPMessage>().eq(WechatMPMessage::getSceneCode, scene).last("LIMIT 1"), false);
        if (mp != null) {
            mp.setCode(code);
            if (enable != null) mp.setEnable(enable);
            return wechatMPMessageService.updateById(mp);
        } else {
            mp = new WechatMPMessage();
            mp.setSceneCode(scene);
            mp.setName(notice.getNoticeTitle());
            mp.setCode(code);
            mp.setEnable(enable != null ? enable : true);
            return wechatMPMessageService.save(mp);
        }
    }

    private String resolveSceneCode(NoticeMessage notice) {
        if (CharSequenceUtil.isNotBlank(notice.getSceneCode())) {
            return notice.getSceneCode();
        }
        NoticeMessageNodeEnum en = NoticeMessageNodeResolver.fromNoticeNodeLabel(notice.getNoticeNode());
        return en != null ? en.name() : null;
    }

}
