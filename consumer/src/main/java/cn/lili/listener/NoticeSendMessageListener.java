package cn.lili.listener;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.dos.Message;
import cn.lili.modules.message.entity.dos.StoreMessage;
import cn.lili.modules.message.entity.enums.MessageSendClient;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.entity.enums.RangeEnum;
import cn.lili.modules.message.service.MemberMessageService;
import cn.lili.modules.message.service.StoreMessageService;
import cn.lili.modules.sms.SmsUtil;
import cn.lili.modules.sms.entity.dto.SmsReachDTO;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import cn.lili.rocketmq.tags.OtherTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
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
@RocketMQMessageListener(topic = "${lili.data.rocketmq.notice-send-topic}", consumerGroup = "${lili.data.rocketmq.notice-send-group}")
public class NoticeSendMessageListener implements RocketMQListener<MessageExt> {

    /**
     * 短信
     */
    @Autowired
    private SmsUtil smsUtil;
    /**
     * 店铺消息
     */
    @Autowired
    private StoreMessageService storeMessageService;
    /**
     * 会员消息
     */
    @Autowired
    private MemberMessageService memberMessageService;
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;

    @Override
    public void onMessage(MessageExt messageExt) {
        switch (OtherTagsEnum.valueOf(messageExt.getTags())) {
            case SMS:
                String smsJsonStr = new String(messageExt.getBody());
                SmsReachDTO smsReachDTO = JSONUtil.toBean(smsJsonStr, SmsReachDTO.class);
                //发送全部会员
                if (smsReachDTO.getSmsRange().equals(RangeEnum.ALL.name())) {
                    //获取所有会员的手机号
                    List<String> list = memberService.getAllMemberMobile();
                    smsUtil.sendBatchSms(smsReachDTO.getSignName(), list, smsReachDTO.getMessageCode());
                    //判断为发送部分用户
                } else {
                    smsUtil.sendBatchSms(smsReachDTO.getSignName(), smsReachDTO.getMobile(), smsReachDTO.getMessageCode());
                }
                break;
            //管理员发送站内信
            case MESSAGE:
                Message message = JSONUtil.toBean(new String(messageExt.getBody()), Message.class);
                // 管理端发送给商家的站内信
                if (message.getMessageClient().equals(MessageSendClient.STORE.name().toLowerCase())) {
                    saveStoreMessage(message);
                } else {
                    //管理员发送给会员的站内信
                    saveMemberMessage(message);
                }
                break;
            default:
                break;
        }
    }

    /**
     * 保存店铺消息
     *
     * @param message 消息
     */
    private void saveStoreMessage(Message message) {
        List<StoreMessage> list = new ArrayList<>();
        //发送全部商家情况
        if ("ALL".equals(message.getMessageRange())) {
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
            //发送给指定商家情况
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
        if (list.size() > 0) {
            //执行保存
            storeMessageService.save(list);
        }
    }

    /**
     * 保存会员消息
     *
     * @param message 消息
     */
    private void saveMemberMessage(Message message) {
        List<MemberMessage> list = new ArrayList<>();
        //如果是给所有会员发送消息
        if ("ALL".equals(message.getMessageRange())) {
            //查询所有会员总数，因为会员总数比较大 如果一次性查出来会占用数据库资源，所以要分页查询
            MemberSearchVO memberSearchVO = new MemberSearchVO();
            memberSearchVO.setDisabled(SwitchEnum.OPEN.name());
            long memberNum = memberService.getMemberNum(memberSearchVO);
            //构建分页查询参数
            //100条查一次
            int pageSize = 100;
            int pageCount;
            pageCount = (int) (memberNum / pageSize);
            pageCount = memberNum % pageSize > 0 ? pageCount + 1 : pageCount;
            for (int i = 1; i <= pageCount; i++) {
                PageVO pageVO = new PageVO();
                pageVO.setPageSize(pageSize);
                pageVO.setPageNumber(i);
                IPage<MemberVO> page = memberService.getMemberPage(memberSearchVO, pageVO);
                //循环要保存的信息
                page.getRecords().forEach(item -> {
                    MemberMessage memberMessage = new MemberMessage();
                    memberMessage.setContent(message.getContent());
                    memberMessage.setTitle(message.getTitle());
                    memberMessage.setMessageId(message.getId());
                    memberMessage.setMemberId(item.getId());
                    memberMessage.setMemberName(item.getUsername());
                    memberMessage.setStatus(MessageStatusEnum.UN_READY.name());
                    list.add(memberMessage);
                });
            }

        } else {
            //如果是给指定会员发送消息
            int i = 0;
            for (String str : message.getUserIds()) {
                MemberMessage memberMessage = new MemberMessage();
                memberMessage.setMessageId(message.getId());
                memberMessage.setMemberId(str);
                memberMessage.setMemberName(message.getUserNames()[i]);
                memberMessage.setStatus(MessageStatusEnum.UN_READY.name());
                memberMessage.setTitle(message.getTitle());
                memberMessage.setContent(message.getContent());
                list.add(memberMessage);
                i++;
            }
        }
        if (list.size() > 0) {
            //执行保存
            memberMessageService.save(list);
        }

    }


}
