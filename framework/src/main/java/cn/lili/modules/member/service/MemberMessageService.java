package cn.lili.modules.member.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberMessage;
import cn.lili.modules.member.entity.vo.MemberMessageQueryVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 消息发送业务层
 *
 * @author Chopper
 * @date 2020/11/17 3:44 下午
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


}