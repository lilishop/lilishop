package cn.lili.event.impl;

import cn.lili.event.MemberInfoChangeEvent;
import cn.lili.event.StoreSettingChangeEvent;
import cn.lili.modules.im.entity.dos.ImTalk;
import cn.lili.modules.im.service.ImTalkService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.store.entity.dos.Store;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * Im对话消息
 *
 * @author chc
 * @since 2022/6/2114:46
 */
@Service
public class ImTalkExecute implements MemberInfoChangeEvent, StoreSettingChangeEvent {

    @Autowired
    private ImTalkService imTalkService;

    @Override
    public void memberInfoChange(Member member) {
        //当与UserId1相等时
        List<ImTalk> imTalkList1 = imTalkService.list(new LambdaQueryWrapper<ImTalk>().eq(ImTalk::getUserId1, member.getId()));
        for (ImTalk imTalk : imTalkList1) {
            imTalk.setName1(member.getNickName());
            imTalk.setFace1(member.getFace());
        }
        imTalkService.updateBatchById(imTalkList1);
        List<ImTalk> imTalkList2 = imTalkService.list(new LambdaQueryWrapper<ImTalk>().eq(ImTalk::getUserId2, member.getId()));
        for (ImTalk imTalk : imTalkList2) {
            imTalk.setName2(member.getNickName());
            imTalk.setFace2(member.getFace());
        }
        imTalkService.updateBatchById(imTalkList2);
    }

    @Override
    public void storeSettingChange(Store store) {
        //当与UserId1相等时
        List<ImTalk> imTalkList1 = imTalkService.list(new LambdaQueryWrapper<ImTalk>().eq(ImTalk::getUserId1, store.getId()));
        for (ImTalk imTalk : imTalkList1) {
            imTalk.setName1(store.getStoreName());
            imTalk.setFace1(store.getStoreLogo());
        }
        imTalkService.updateBatchById(imTalkList1);
        List<ImTalk> imTalkList2 = imTalkService.list(new LambdaQueryWrapper<ImTalk>().eq(ImTalk::getUserId2, store.getId()));
        for (ImTalk imTalk : imTalkList2) {
            imTalk.setName2(store.getStoreName());
            imTalk.setFace2(store.getStoreLogo());
        }
        imTalkService.updateBatchById(imTalkList2);
    }
}
