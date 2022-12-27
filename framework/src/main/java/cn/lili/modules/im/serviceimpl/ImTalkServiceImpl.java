package cn.lili.modules.im.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.entity.vo.ImTalkVO;
import cn.lili.modules.im.mapper.ImTalkMapper;
import cn.lili.modules.im.service.ImMessageService;
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

import java.util.List;
import java.util.stream.Collectors;

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

    @Autowired
    private ImMessageService imMessageService;

    @Override
    public ImTalk getTalkByUser(String userId1, String userId2) {
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImTalk::getUserId2, userId2);
        queryWrapper.eq(ImTalk::getUserId1, userId1);
        ImTalk imTalk = this.getOne(queryWrapper);
        AuthUser currentUser = UserContext.getCurrentUser();
        //如果没有聊天，则创建聊天
        if (imTalk == null) {
            // 没有登录的这个账户信息
            if (currentUser == null) {
                return null;
            }
            //当自己为店铺时
            if(UserEnums.STORE.equals(currentUser.getRole())){
                Store selfStore = storeService.getById(userId1);
                //没有这个用户信息
                Member other = memberService.getById(userId2);
                if(other == null){
                    return null;
                }
                //自己为店铺其他人必定为用户
                imTalk = new ImTalk(userId1, userId2, selfStore.getStoreLogo(), other.getFace(), selfStore.getStoreName(), other.getNickName());
                imTalk.setStoreFlag1(true);
            }else if(UserEnums.MEMBER.equals(currentUser.getRole())){
                //没有这个店铺信息
                Member self = memberService.getById(userId1);
                Member otherMember = memberService.getById(userId2);
                Store otherStore = storeService.getById(userId2);
                if(otherStore != null){
                    imTalk = new ImTalk(userId1, userId2, self.getFace(), otherStore.getStoreLogo(), self.getNickName(), otherStore.getStoreName());
                    imTalk.setStoreFlag2(true);
                }else if (otherMember != null){
                    imTalk = new ImTalk(userId1, userId2, self.getFace(), otherMember.getFace(), self.getNickName(), otherMember.getNickName());
                }else{
                    return null;
                }
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

    @Override
    public List<ImTalkVO> getUserTalkList(String userName) {
        AuthUser authUser = UserContext.getCurrentUser();
        if(authUser == null){
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.and(wq->{
            wq.like(ImTalk::getName1, userName).or().like(ImTalk::getName2,userName);
        });
        queryWrapper.and(wq->{
            wq.like(ImTalk::getUserId1, authUser.getId()).or().like(ImTalk::getUserId2,authUser.getId());
        });
        queryWrapper.orderByDesc(ImTalk::getLastTalkTime);
        List<ImTalk> imTalks = this.list(queryWrapper);
        List<ImTalkVO> imTalkVOList = imTalks.stream().map(imTalk -> {
            return new ImTalkVO(imTalk, authUser.getId());
        }).collect(Collectors.toList());
        getUnread(imTalkVOList);
        return imTalkVOList;
    }

    @Override
    public List<ImTalkVO> getStoreTalkList() {
        AuthUser authUser = UserContext.getCurrentUser();
        if(authUser == null){
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.and(wq->{
            wq.like(ImTalk::getUserId1, authUser.getStoreId()).or().like(ImTalk::getUserId2,authUser.getStoreId());
        });
        queryWrapper.orderByDesc(ImTalk::getLastTalkTime);
        List<ImTalk> imTalks = this.list(queryWrapper);

        List<ImTalkVO> imTalkVOList = imTalks.stream().map(imTalk -> {
            return new ImTalkVO(imTalk, authUser.getStoreId());
        }).collect(Collectors.toList());
        getUnread(imTalkVOList);
        return imTalkVOList;
    }

    /**
     * 获取未读消息数量
     * @param imTalkVOList
     */
    private void getUnread(List<ImTalkVO> imTalkVOList){
        if(imTalkVOList.size() > 0){
            for (ImTalkVO imTalkVO : imTalkVOList) {
                long count = imMessageService.count(new LambdaQueryWrapper<ImMessage>().eq(ImMessage::getFromUser, imTalkVO.getUserId()).eq(ImMessage::getIsRead, false));
                imTalkVO.setUnread(count);
            }
        }
    }
}