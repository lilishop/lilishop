package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.rocketmq.tags.OtherTagsEnum;
import cn.lili.common.sms.SmsUtil;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.message.entity.dos.Message;
import cn.lili.modules.message.entity.dos.StoreMessage;
import cn.lili.modules.message.entity.dto.SmsReachDTO;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.entity.enums.RangeEnum;
import cn.lili.modules.message.service.StoreMessageService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.RequiredArgsConstructor;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * 消息发送
 *
 * @author paulG
 * @since 2020/12/9
 */
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
@RocketMQMessageListener(topic = "${lili.data.rocketmq.notice-send-topic}", consumerGroup = "${lili.data.rocketmq.notice-send-group}")
public class NoticeSendMessageListener implements RocketMQListener<MessageExt> {

    //会员
    private final MemberMapper memberMapper;
    //短信
    private final SmsUtil smsUtil;
    //店铺消息
    private final StoreMessageService storeMessageService;
    //店铺
    private final StoreService storeService;

    @Override
    public void onMessage(MessageExt messageExt) {
        switch (OtherTagsEnum.valueOf(messageExt.getTags())) {
            case SMS:
                String smsJsonStr = new String(messageExt.getBody());
                SmsReachDTO smsReachDTO = JSONUtil.toBean(smsJsonStr, SmsReachDTO.class);

                //发送全部会员
                if (smsReachDTO.getSmsRange().equals(RangeEnum.ALL.name())) {
                    //获取所有会员的手机号
                    List<String> list = memberMapper.getAllMemberMobile();
                    smsUtil.sendBatchSms(smsReachDTO.getSignName(), list, smsReachDTO.getMessageCode());
                    //判断为发送部分用户
                } else if (smsReachDTO.getSmsRange().equals(RangeEnum.APPOINT.name())) {
                    smsUtil.sendBatchSms(smsReachDTO.getSignName(), smsReachDTO.getMobile(), smsReachDTO.getMessageCode());
                }
                break;
            //管理员发送站内信
            case MESSAGE:
                Message message = JSONUtil.toBean(new String(messageExt.getBody()), Message.class);
                List<StoreMessage> list = new ArrayList<>();
                //保存商家记录
                if (message.getMessageRange().equals("ALL")) {
                    List<Store> storeList = storeService.list(new QueryWrapper<Store>().eq("store_disable", "OPEN"));
                    storeList.forEach(item -> {
                        StoreMessage storeMessage = new StoreMessage();
                        storeMessage.setMessageId(message.getId());
                        storeMessage.setStoreName(item.getStoreName());
                        storeMessage.setStoreId(item.getId());
                        storeMessage.setStatus(MessageStatusEnum.UN_READY.name());
                        storeMessage.setTitle(message.getTitle());
                        storeMessage.setContent(message.getContent());
                        list.add(storeMessage);
                    });
                } else {
                    int i = 0;
                    for (String str : message.getUserIds()) {
                        StoreMessage storeMessage = new StoreMessage();
                        storeMessage.setMessageId(message.getId());
                        storeMessage.setStoreId(str);
                        storeMessage.setStoreName(message.getUserNames()[i]);
                        storeMessage.setStatus(MessageStatusEnum.UN_READY.name());
                        storeMessage.setTitle(message.getTitle());
                        storeMessage.setContent(message.getContent());
                        list.add(storeMessage);
                        i++;
                    }
                }
                storeMessageService.save(list);
                break;
            default:
                break;
        }
    }
}
