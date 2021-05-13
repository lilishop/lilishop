package cn.lili.modules.member.serviceimpl;


import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberMessage;
import cn.lili.modules.member.entity.vo.MemberMessageQueryVO;
import cn.lili.modules.member.mapper.MemberMessageMapper;
import cn.lili.modules.member.service.MemberMessageService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

/**
 * 会员消息业务层实现
 *
 * @author Chopper
 * @date 2020/11/17 3:48 下午
 */
@Service
@Transactional
public class MemberMessageServiceImpl extends ServiceImpl<MemberMessageMapper, MemberMessage> implements MemberMessageService {


    @Override
    public IPage<MemberMessage> getPage(MemberMessageQueryVO memberMessageQueryVO, PageVO pageVO) {
        QueryWrapper<MemberMessage> queryWrapper = new QueryWrapper<>();
        //消息标题
        queryWrapper.like(StringUtils.isNotEmpty(memberMessageQueryVO.getTitle()), "title", memberMessageQueryVO.getTitle());
        //会员id
        queryWrapper.like(StringUtils.isNotEmpty(memberMessageQueryVO.getMemberId()), "member_id", memberMessageQueryVO.getMemberId());
        //消息状态
        queryWrapper.like(StringUtils.isNotEmpty(memberMessageQueryVO.getStatus()), "status", memberMessageQueryVO.getStatus());
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
}