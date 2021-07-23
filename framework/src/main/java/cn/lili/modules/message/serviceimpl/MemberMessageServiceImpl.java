package cn.lili.modules.message.serviceimpl;


import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.message.entity.dos.MemberMessage;
import cn.lili.modules.message.mapper.MemberMessageMapper;
import cn.lili.modules.message.service.MemberMessageService;
import cn.lili.modules.message.entity.vos.MemberMessageQueryVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 会员接收消息业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 3:48 下午
 */
@Service
public class MemberMessageServiceImpl extends ServiceImpl<MemberMessageMapper, MemberMessage> implements MemberMessageService {


    @Override
    public IPage<MemberMessage> getPage(MemberMessageQueryVO memberMessageQueryVO, PageVO pageVO) {
        QueryWrapper<MemberMessage> queryWrapper = new QueryWrapper<>();
        //消息id
        queryWrapper.eq(StringUtils.isNotEmpty(memberMessageQueryVO.getMessageId()), "message_id", memberMessageQueryVO.getMessageId());
        //消息标题
        queryWrapper.like(StringUtils.isNotEmpty(memberMessageQueryVO.getTitle()), "title", memberMessageQueryVO.getTitle());
        //会员id
        queryWrapper.eq(StringUtils.isNotEmpty(memberMessageQueryVO.getMemberId()), "member_id", memberMessageQueryVO.getMemberId());
        //消息状态
        queryWrapper.eq(StringUtils.isNotEmpty(memberMessageQueryVO.getStatus()), "status", memberMessageQueryVO.getStatus());
        //倒序
        queryWrapper.orderByDesc("create_time");
        //构建查询
        return this.page(PageUtil.initPage(pageVO), queryWrapper);
    }

    @Override
    public Boolean editStatus(String status, String messageId) {
        //查询消息是否存在
        MemberMessage memberMessage = this.getById(messageId);
        if (memberMessage != null) {
            memberMessage.setStatus(status);
            //执行修改
            return this.updateById(memberMessage);
        }
        return false;
    }


    @Override
    public Boolean deleteMessage(String messageId) {
        //查询消息是否存在
        MemberMessage memberMessage = this.getById(messageId);
        if (memberMessage != null) {
            //执行删除
            return this.removeById(memberMessage);
        }
        return false;
    }

    @Override
    public boolean save(List<MemberMessage> messages) {
        return saveBatch(messages);
    }
}