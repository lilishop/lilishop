package cn.lili.im.serviceimpl;

import cn.lili.common.security.context.UserContext;
import cn.lili.im.entity.ImMessage;
import cn.lili.im.mapper.ImMessageMapper;
import cn.lili.im.service.ImMessageService;
import cn.lili.im.service.ImTalkService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

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
}