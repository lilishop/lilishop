package cn.lili.modules.message.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.NoticeMessage;
import cn.lili.modules.message.entity.dto.MessageTemplateAggregateDTO;
import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 通知类消息模板业务层
 *
 * @author Bulbasaur
 * @since 2020/12/8 9:47
 */
public interface NoticeMessageService extends IService<NoticeMessage> {

    /**
     * 多条件分页获取
     *
     * @param pageVO 分页数据
     * @param type   类型 (MEMBER / STORE / PLATFORM)
     * @return 消息模板分页
     */
    IPage<NoticeMessage> getMessageTemplate(PageVO pageVO, String type);

    /**
     * 根据模板编码获取消息模板
     *
     * @param noticeMessageDTO 站内信消息
     */
    void noticeMessage(NoticeMessageDTO noticeMessageDTO);

    /**
     * 分页聚合站内信、短信与微信服务号/小程序模板（按 scene_code 关联）。
     */
    IPage<MessageTemplateAggregateDTO> getMessageTemplateAggregatePage(PageVO pageVO, String type);

    /**
     * 快速更新指定渠道开关状态
     *
     * @param id      通知消息ID
     * @param channel 渠道: NOTICE / SMS / WECHAT_OA / WECHAT_MP
     * @param status  状态: OPEN / CLOSE
     * @return 是否成功
     */
    boolean updateChannelSwitch(String id, String channel, String status);

    /**
     * 更新站内信模板内容及开关
     *
     * @param id      通知消息ID
     * @param title   标题
     * @param content 内容
     * @param status  状态 (OPEN/CLOSE)
     * @return 是否成功
     */
    boolean updateNoticeTemplate(String id, String title, String content, String status);

    /**
     * 更新短信模板ID及开关 (内容只读不可编辑)
     *
     * @param id              通知消息ID
     * @param smsTemplateCode 短信模板CODE
     * @param status          状态 (OPEN/CLOSE)
     * @return 是否成功
     */
    boolean updateSmsTemplate(String id, String smsTemplateCode, String status);

    /**
     * 更新微信服务号模板ID及开关 (格式只读不可编辑)
     *
     * @param id     通知消息ID
     * @param code   微信模板Code
     * @param enable 启用状态
     * @return 是否成功
     */
    boolean updateWechatOaTemplate(String id, String code, Boolean enable);

    /**
     * 更新微信小程序订阅消息模板ID及开关 (格式只读不可编辑)
     *
     * @param id     通知消息ID
     * @param code   微信小程序订阅模板Code
     * @param enable 启用状态
     * @return 是否成功
     */
    boolean updateWechatMpTemplate(String id, String code, Boolean enable);

}
