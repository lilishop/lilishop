package cn.lili.modules.im.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.im.entity.dos.ImMessage;
import cn.lili.modules.im.entity.dto.MessageQueryParams;
import cn.lili.modules.im.mapper.ImMessageMapper;
import cn.lili.modules.im.service.ImMessageService;
import cn.lili.modules.im.service.ImTalkService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;

/**
 * Im消息 业务实现
 *
 * @author Chopper
 */
@Service
@Transactional(rollbackFor = Exception.class)
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class ImMessageServiceImpl extends ServiceImpl<ImMessageMapper, ImMessage> implements ImMessageService {

    @Autowired
    private ImTalkService imTalkService;

    @Override
    public void read(String talkId, String accessToken) {
        LambdaUpdateWrapper<ImMessage> updateWrapper = new LambdaUpdateWrapper<>();
        String userId = UserContext.getAuthUser(accessToken).getId();
        updateWrapper.eq(ImMessage::getTalkId, talkId);
        updateWrapper.eq(ImMessage::getToUser, userId);
        updateWrapper.set(ImMessage::getIsRead, true);
        this.update(updateWrapper);
    }

    @Override
    public List<ImMessage> unReadMessages(String accessToken) {
        String userId = UserContext.getAuthUser(accessToken).getId();
        LambdaQueryWrapper<ImMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImMessage::getToUser, userId);
        queryWrapper.eq(ImMessage::getIsRead, false);
        return this.list(queryWrapper);
    }

    @Override
    public List<ImMessage> historyMessage(String accessToken, String to) {
        String userId = UserContext.getAuthUser(accessToken).getId();
        LambdaQueryWrapper<ImMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.and(i -> i.eq(ImMessage::getToUser, userId).and(j -> j.eq(ImMessage::getFromUser, to)));
        queryWrapper.or(i -> i.eq(ImMessage::getToUser, to).and(j -> j.eq(ImMessage::getFromUser, userId)));
        queryWrapper.orderByDesc(ImMessage::getCreateTime);
        return this.list(queryWrapper);
    }

    @Override
    public Boolean hasNewMessage(String accessToken) {
        String userId = UserContext.getAuthUser(accessToken).getId();
        LambdaQueryWrapper<ImMessage> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImMessage::getIsRead, false);
        queryWrapper.eq(ImMessage::getToUser, userId);
        return this.list(queryWrapper).size() > 0;

    }

    @Override
    public List<ImMessage> getList(MessageQueryParams messageQueryParams) {
        List<ImMessage> messageList = this.page(PageUtil.initPage(messageQueryParams), messageQueryParams.initQueryWrapper()).getRecords();
        ListSort(messageList);
        readMessage(messageList);
        return messageList;
    }

    @Override
    public Long unreadMessageCount() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if(currentUser == null){
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        return this.count(new LambdaQueryWrapper<ImMessage>().eq(ImMessage::getToUser,currentUser.getId()).eq(ImMessage::getIsRead,false));
    }

    @Override
    public void cleanUnreadMessage() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if(currentUser == null){
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        this.update(new LambdaUpdateWrapper<ImMessage>().eq(ImMessage::getToUser,currentUser.getId()).set(ImMessage::getIsRead,true));
    }

    /**
     * 根据时间倒叙
     *
     * @param list
     */
    private static void ListSort(List<ImMessage> list) {
        list.sort(new Comparator<ImMessage>() {
            @Override
            public int compare(ImMessage e1, ImMessage e2) {
                try {
                    if (e1.getCreateTime().before(e2.getCreateTime())) {
                        return -1;
                    } else {
                        return 1;
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return 0;
            }
        });
    }


    /**
     * 阅读消息
     *
     * @param messageList 消息列表
     */
    private void readMessage(List<ImMessage> messageList) {
        if (!messageList.isEmpty()) {
            //判断用户类型
            AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
            String toUserId = "";
            if(UserEnums.MEMBER.equals(authUser.getRole())){
                toUserId = authUser.getId();
            }else if(UserEnums.STORE.equals(authUser.getRole())){
                toUserId = authUser.getStoreId();
            }
            //发送给自己的未读信息进行已读操作
            for (ImMessage imMessage : messageList) {
                if(Boolean.FALSE.equals(imMessage.getIsRead()) && imMessage.getToUser().equals(toUserId)){
                    imMessage.setIsRead(true);
                }
            }
        }
        this.updateBatchById(messageList);
    }

}