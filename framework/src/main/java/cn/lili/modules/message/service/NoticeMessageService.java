package cn.lili.modules.message.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.NoticeMessage;
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
     * @param type   类型
     * @return
     */
    IPage<NoticeMessage> getMessageTemplate(PageVO pageVO, String type);

    /**
     * 根据模板编码获取消息模板
     *
     * @param noticeMessageDTO 站内信消息
     */
    void noticeMessage(NoticeMessageDTO noticeMessageDTO);

}