package cn.lili.modules.im.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.mapper.ImTalkMapper;
import cn.lili.modules.im.service.ImTalkService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.stringtemplate.v4.ST;

/**
 * 聊天 业务实现
 *
 * @author Chopper
 */
@Service
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImTalkServiceImpl extends ServiceImpl<ImTalkMapper, ImTalk> implements ImTalkService {

    @Autowired
    private MemberService memberService;

    @Autowired
    private StoreService storeService;

    @Override
    public ImTalk getTalkByUser(String userId1, String userId2) {
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImTalk::getUserId2, userId2);
        queryWrapper.eq(ImTalk::getUserId1, userId1);
        ImTalk imTalk = this.getOne(queryWrapper);
        Member self = memberService.getById(userId1);
        Store selfStore = storeService.getById(userId1);


        //如果没有聊天，则创建聊天
        if (imTalk == null) {
            // 没有登录的这个账户信息
            if (self == null && selfStore ==null) {
                return null;
            }
            //当自己为店铺时
            if(selfStore != null){
                //没有这个用户信息
                Member other = memberService.getById(userId2);
                if(other == null){
                    return null;
                }
                //自己为店铺其他人必定为用户
                imTalk = new ImTalk(userId1, userId2, selfStore.getStoreLogo(), other.getFace(), selfStore.getStoreName(), other.getNickName());
            }else if(self != null){
                //没有这个店铺信息
                Store otherStore = storeService.getById(userId2);
                if(otherStore == null){
                    return null;
                }
                //当自己为用户时 其他人必定为店铺
                imTalk = new ImTalk(userId1, userId2, self.getFace(), otherStore.getStoreLogo(), self.getNickName(), otherStore.getStoreName());
            }
            this.save(imTalk);
        } else {
            imTalk = check(imTalk);
        }
        return imTalk;
    }

    /**
     * 发起聊天后，如果聊天不可见为true，则需要修正
     *
     * @param imTalk
     */
    private ImTalk check(ImTalk imTalk) {
        if (imTalk.getDisable1() || imTalk.getDisable2()) {
            imTalk.setDisable1(false);
            imTalk.setDisable2(false);
            this.updateById(imTalk);

        }
        return imTalk;
    }

    @Override
    public void top(String id, Boolean top) {
        ImTalk imTalk = this.getById(id);
        if (imTalk.getUserId1().equals(UserContext.getCurrentUser().getId())) {
            imTalk.setTop1(top);
        } else if (imTalk.getUserId2().equals(UserContext.getCurrentUser().getId())) {
            imTalk.setTop2(top);
        } else {
            throw new ServiceException(ResultCode.ERROR);
        }
        this.updateById(imTalk);
    }

    @Override
    public void disable(String id) {
        ImTalk imTalk = this.getById(id);
        if (imTalk.getUserId1().equals(UserContext.getCurrentUser().getId())) {
            imTalk.setDisable1(true);
            this.updateById(imTalk);
        } else if (imTalk.getUserId2().equals(UserContext.getCurrentUser().getId())) {
            imTalk.setDisable2(true);
            this.updateById(imTalk);
        }
    }
}