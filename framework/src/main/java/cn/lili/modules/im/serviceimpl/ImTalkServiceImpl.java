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

import java.util.List;
import java.util.Objects;
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

    public ImTalk getTalkByUser(String userId) {
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        AuthUser currentUser = Objects.requireNonNull(UserContext.getCurrentUser());
        //登录用户的Id
        String selfId = "";
        //查看当前用户角色对Id进行赋值
        if(UserEnums.STORE.equals(currentUser.getRole())){
            selfId = currentUser.getStoreId();
        }else if(UserEnums.MEMBER.equals(currentUser.getRole())){
            selfId = currentUser.getId();
        }
        //小数在前保证永远是同一个对话
        String finalSelfId = selfId;
        queryWrapper.and(wq-> wq.eq(ImTalk::getUserId2, userId).eq(ImTalk::getUserId1, finalSelfId).or().eq(ImTalk::getUserId2, finalSelfId).eq(ImTalk::getUserId1, userId));
        ImTalk imTalk = this.getOne(queryWrapper);
        //如果没有聊天，则创建聊天
        if (imTalk == null) {
            //当自己为店铺时
            if(UserEnums.STORE.equals(currentUser.getRole())){
                Store selfStore = storeService.getById(selfId);
                //没有这个用户信息
                Member other = memberService.getById(userId);
                if(other == null){
                    return null;
                }
                //自己为店铺其他人必定为用户
                imTalk = new ImTalk(other,selfStore);
            }else if(UserEnums.MEMBER.equals(currentUser.getRole())){
                //没有这个店铺信息
                Member self = memberService.getById(selfId);
                Member otherMember = memberService.getById(userId);
                Store otherStore = storeService.getById(userId);
                if(otherStore != null){
                    imTalk = new ImTalk(self, otherStore);
                }else if (otherMember != null){
                    imTalk = new ImTalk(self, otherMember);
                }
            }
            this.save(imTalk);
        }
        return imTalk;
    }

    /**
     * 发起聊天后，如果聊天不可见为true，则需要修正
     *
     * @param imTalk 对话信息
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
    public List<ImTalkVO> getUserTalkList() {
        AuthUser authUser = UserContext.getCurrentUser();
        if(authUser == null){
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.and(wq-> wq.eq(ImTalk::getUserId1, authUser.getId()).or().eq(ImTalk::getUserId2,authUser.getId()));
        queryWrapper.orderByDesc(ImTalk::getLastTalkTime);
        List<ImTalk> imTalks = this.list(queryWrapper);
        List<ImTalkVO> imTalkVOList = imTalks.stream().map(imTalk -> new ImTalkVO(imTalk, authUser.getId())).collect(Collectors.toList());
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
            wq.eq(ImTalk::getUserId1, authUser.getStoreId()).or().eq(ImTalk::getUserId2,authUser.getStoreId());
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
     * @param imTalkVOList 消息列表
     */
    private void getUnread(List<ImTalkVO> imTalkVOList){
        if(!imTalkVOList.isEmpty()){
            for (ImTalkVO imTalkVO : imTalkVOList) {
                long count = imMessageService.count(new LambdaQueryWrapper<ImMessage>().eq(ImMessage::getFromUser, imTalkVO.getUserId()).eq(ImMessage::getTalkId,imTalkVO.getId()).eq(ImMessage::getIsRead, false));
                imTalkVO.setUnread(count);
            }
        }
    }
}