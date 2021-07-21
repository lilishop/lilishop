package cn.lili.modules.message.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.entity.vos.MemberMessageQueryVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 会员消息发送业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:44 下午
 */
public interface MemberMessageService extends IService<MemberMessage> {

    /**
     * 会员消息查询接口
     *
     * @param memberMessageQueryVO 会员查询条件
     * @param pageVO               分页条件
     * @return 会员消息分页
     */
    IPage<MemberMessage> getPage(MemberMessageQueryVO memberMessageQueryVO, PageVO pageVO);

    /**
     * 修改会员消息状态
     *
     * @param status    状态
     * @param messageId 消息id
     * @return 操作状态
     */
    Boolean editStatus(String status, String messageId);

    /**
     * 删除消息
     *
     * @param messageId 消息id
     * @return 操作状态
     */
    Boolean deleteMessage(String messageId);


    /**
     * 保存消息信息
     *
     * @param messages 消息
     * @return
     */
    boolean save(List<MemberMessage> messages);


}