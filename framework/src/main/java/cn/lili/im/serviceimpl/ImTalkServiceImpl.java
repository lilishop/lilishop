package cn.lili.im.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.im.entity.ImTalk;
import cn.lili.im.entity.ImUser;
import cn.lili.im.mapper.ImTalkMapper;
import cn.lili.im.service.ImTalkService;
import cn.lili.im.service.ImUserService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

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
    private ImUserService imUserService;

    @Override
    public ImTalk getTalkByUser(String userId1, String userId2) {
        LambdaQueryWrapper<ImTalk> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(ImTalk::getUserId2, userId2);
        queryWrapper.eq(ImTalk::getUserId1, userId1);
        ImTalk imTalk = this.getOne(queryWrapper);
        ImUser imUser1 = imUserService.getById(userId1);
        ImUser imUser2 = imUserService.getById(userId2);

        //如果没有聊天，则创建聊天
        if (imTalk == null) {
            if (imUser1 == null || imUser2 == null) {
                return null;
            }
            imTalk = new ImTalk(userId1, userId2, imUser1.getFace(), imUser2.getFace(), imUser1.getName(), imUser2.getName());
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