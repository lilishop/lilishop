package cn.lili.modules.member.serviceimpl;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberGroupUser;
import cn.lili.modules.member.mapper.MemberGroupUserMapper;
import cn.lili.modules.member.service.MemberGroupUserService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import cn.lili.mybatis.util.PageUtil;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

@Service
public class MemberGroupUserServiceImpl extends ServiceImpl<MemberGroupUserMapper, MemberGroupUser> implements MemberGroupUserService {
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateGroupUsers(String groupId, List<String> memberIds) {
        if (memberIds == null || memberIds.isEmpty()) {
            return true;
        }
        QueryWrapper<MemberGroupUser> existsWrapper = new QueryWrapper<>();
        existsWrapper.in("member_id", memberIds);
        List<MemberGroupUser> exists = this.list(existsWrapper);
        List<String> existingMemberIds = new ArrayList<>();
        for (MemberGroupUser e : exists) {
            existingMemberIds.add(e.getMemberId());
        }
        List<MemberGroupUser> toAdd = new ArrayList<>();
        for (String memberId : memberIds) {
            if (existingMemberIds.contains(memberId)) {
                continue;
            }
            MemberGroupUser rel = new MemberGroupUser();
            rel.setGroupId(groupId);
            rel.setMemberId(memberId);
            toAdd.add(rel);
        }
        if (toAdd.isEmpty()) {
            return true;
        }
        return this.saveBatch(toAdd);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateUserGroups(String memberId, List<String> groupIds) {
        QueryWrapper<MemberGroupUser> wrapper = new QueryWrapper<>();
        wrapper.eq("member_id", memberId);
        this.remove(wrapper);
        if (groupIds == null || groupIds.isEmpty()) {
            return true;
        }
        List<MemberGroupUser> list = new ArrayList<>();
        for (String groupId : groupIds) {
            MemberGroupUser rel = new MemberGroupUser();
            rel.setGroupId(groupId);
            rel.setMemberId(memberId);
            list.add(rel);
        }
        return this.saveBatch(list);
    }

    @Override
    public long countByGroupId(String groupId) {
        QueryWrapper<MemberGroupUser> wrapper = new QueryWrapper<>();
        wrapper.eq("group_id", groupId);
        return this.count(wrapper);
    }

    @Override
    public IPage<MemberGroupUser> pageByGroupId(String groupId, PageVO pageVO) {
        QueryWrapper<MemberGroupUser> wrapper = new QueryWrapper<>();
        wrapper.eq("group_id", groupId);
        return this.page(PageUtil.initPage(pageVO), wrapper);
    }

    @Override
    public boolean removeByGroupAndMember(String groupId, String memberId) {
        QueryWrapper<MemberGroupUser> wrapper = new QueryWrapper<>();
        wrapper.eq("group_id", groupId).eq("member_id", memberId);
        return this.remove(wrapper);
    }
}
