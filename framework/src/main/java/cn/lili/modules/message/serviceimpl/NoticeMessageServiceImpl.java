package cn.lili.modules.message.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum;
import cn.lili.modules.message.mapper.NoticeMessageTemplateMapper;
import cn.lili.modules.message.service.MemberMessageService;
import cn.lili.modules.message.service.NoticeMessageService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

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

    @Override
    public IPage<NoticeMessage> getMessageTemplate(PageVO pageVO, String type) {
        //构建查询参数
        QueryWrapper<NoticeMessage> messageTemplateQueryWrapper = new QueryWrapper<>();
        //消息模板类型
        messageTemplateQueryWrapper.eq(!CharSequenceUtil.isEmpty(type), "type", type);
        messageTemplateQueryWrapper.orderByDesc("create_time");
        //查询数据返回
        return this.page(PageUtil.initPage(pageVO), messageTemplateQueryWrapper);

    }

    @Override
    public void noticeMessage(NoticeMessageDTO noticeMessageDTO) {
        if (noticeMessageDTO == null) {
            return;
        }
        try {
            NoticeMessage noticeMessage = this.getOne(
                    new LambdaQueryWrapper<NoticeMessage>()
                            .eq(NoticeMessage::getNoticeNode
                                    , noticeMessageDTO.getNoticeMessageNodeEnum().getDescription().trim()));
            //如果通知类站内信开启的情况下
            if (noticeMessage != null && noticeMessage.getNoticeStatus().equals(SwitchEnum.OPEN.name())) {
                MemberMessage memberMessage = new MemberMessage();
                memberMessage.setMemberId(noticeMessageDTO.getMemberId());
                memberMessage.setTitle(noticeMessage.getNoticeTitle());
                memberMessage.setContent(noticeMessage.getNoticeContent());
                //参数不为空，替换内容
                if (noticeMessageDTO.getParameter() != null) {
                    memberMessage.setContent(replaceNoticeContent(noticeMessage.getNoticeContent(), noticeMessageDTO.getParameter()));
                } else {
                    memberMessage.setContent(noticeMessage.getNoticeContent());
                }
                memberMessage.setStatus(MessageStatusEnum.UN_READY.name());
                //添加站内信
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
            String description = NoticeMessageParameterEnum.getValueByType(entry.getKey());
            if (description != null && entry.getValue() != null) {
                noticeContent = noticeContent.replace("#{" + description + "}".trim(), entry.getValue());
            }
        }
        return noticeContent;
    }


}