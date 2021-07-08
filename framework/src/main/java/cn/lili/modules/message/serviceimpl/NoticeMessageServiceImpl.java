package cn.lili.modules.message.serviceimpl;

import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberNotice;
import cn.lili.modules.member.service.MemberNoticeService;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum;
import cn.lili.modules.message.mapper.NoticeMessageTemplateMapper;
import cn.lili.modules.message.service.NoticeMessageService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Map;

/**
 * 通知类消息模板业务层实现
 *
 * @author Bulbasaur
 * @date 2020/12/8 9:48
 */
@Service
public class NoticeMessageServiceImpl extends ServiceImpl<NoticeMessageTemplateMapper, NoticeMessage> implements NoticeMessageService {

    @Autowired
    private MemberNoticeService memberNoticeService;

    @Override
    public IPage<NoticeMessage> getMessageTemplate(PageVO pageVO, String type) {
        //构建查询参数
        QueryWrapper<NoticeMessage> messageTemplateQueryWrapper = new QueryWrapper<>();
        //消息模板类型
        messageTemplateQueryWrapper.eq(!StringUtils.isEmpty(type), "type", type);
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
            NoticeMessage noticeMessage = this.getOne(new LambdaQueryWrapper<NoticeMessage>().eq(NoticeMessage::getNoticeNode, noticeMessageDTO.getNoticeMessageNodeEnum().getDescription().trim()));
            //如果通知类站内信开启的情况下
            if (noticeMessage != null && noticeMessage.getNoticeStatus().equals(SwitchEnum.OPEN.name())) {
                MemberNotice memberNotice = new MemberNotice();
                memberNotice.setMemberId(noticeMessageDTO.getMemberId());
                memberNotice.setTitle(noticeMessage.getNoticeTitle());
                memberNotice.setContent(noticeMessage.getNoticeContent());
                //参数不为空，替换内容
                if (noticeMessageDTO.getParameter() != null) {
                    memberNotice.setContent(replaceNoticeContent(noticeMessage.getNoticeContent(), noticeMessageDTO.getParameter()));
                } else {
                    memberNotice.setContent(noticeMessage.getNoticeContent());
                }
                //添加站内信
                memberNoticeService.save(memberNotice);
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
        for (String key : parameter.keySet()) {
            String description = NoticeMessageParameterEnum.getValueByType(key);
            if (description != null && parameter.get(key) != null) {
                noticeContent = noticeContent.replace("#{" + description + "}".trim(), parameter.get(key));
            }
        }
        return noticeContent;
    }


}