package cn.lili.modules.member.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.common.sensitive.SensitiveWordsHelper;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberGroupUser;
import cn.lili.modules.member.entity.dto.ManagerMemberEditDTO;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.service.MemberGroupUserService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;

/**
 * 客户管理端运维与多维检索支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
public class MemberManageSupport {

    @Autowired
    private MemberGroupUserService memberGroupUserService;

    public MemberManageSupport() {
    }

    public MemberManageSupport(MemberGroupUserService memberGroupUserService) {
        this.memberGroupUserService = memberGroupUserService;
    }

    /**
     * 分页查询客户列表
     *
     * @param memberMapper   客户 Mapper
     * @param memberSearchVO 搜索参数
     * @param page           分页参数
     * @return 客户 VO 分页结果
     */
    public IPage<MemberVO> getMemberPage(MemberMapper memberMapper, MemberSearchVO memberSearchVO, PageVO page) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getUsername()), "m.username", memberSearchVO.getUsername());
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getNickName()), "m.nick_name", memberSearchVO.getNickName());
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getMobile()), "m.mobile", memberSearchVO.getMobile());
        queryWrapper.eq(CharSequenceUtil.isNotBlank(memberSearchVO.getGradeId()), "m.grade_id", memberSearchVO.getGradeId());
        queryWrapper.ge(memberSearchVO.getMinPoint() != null, "m.point", memberSearchVO.getMinPoint());
        queryWrapper.le(memberSearchVO.getMaxPoint() != null, "m.point", memberSearchVO.getMaxPoint());
        queryWrapper.eq(CharSequenceUtil.isNotBlank(memberSearchVO.getId()), "m.id", memberSearchVO.getId());
        if (CharSequenceUtil.isNotBlank(memberSearchVO.getDisabled())) {
            queryWrapper.eq("m.disabled", SwitchEnum.OPEN.name().equals(memberSearchVO.getDisabled()) ? 1 : 0);
        }
        if (CharSequenceUtil.isNotBlank(memberSearchVO.getGroupId()) && memberGroupUserService != null) {
            List<String> groupMemberIds = memberGroupUserService.listObjs(
                    new LambdaQueryWrapper<MemberGroupUser>()
                            .eq(MemberGroupUser::getGroupId, memberSearchVO.getGroupId())
                            .eq(MemberGroupUser::getDeleteFlag, false),
                    String::valueOf);
            if (groupMemberIds.isEmpty()) {
                queryWrapper.eq("m.id", -1);
            } else {
                queryWrapper.in("m.id", groupMemberIds);
            }
        }
        queryWrapper.orderByDesc("m.create_time");
        return memberMapper.pageByMemberVO(PageUtil.initPage(page), queryWrapper);
    }

    /**
     * 根据搜索条件统计客户总数
     *
     * @param memberMapper   客户 Mapper
     * @param memberSearchVO 搜索参数
     * @return 客户总数
     */
    public long getMemberNum(MemberMapper memberMapper, MemberSearchVO memberSearchVO) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getUsername()), "username", memberSearchVO.getUsername());
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getMobile()), "mobile", memberSearchVO.getMobile());
        queryWrapper.ge(memberSearchVO.getMinPoint() != null, "point", memberSearchVO.getMinPoint());
        queryWrapper.le(memberSearchVO.getMaxPoint() != null, "point", memberSearchVO.getMaxPoint());
        if (CharSequenceUtil.isNotBlank(memberSearchVO.getDisabled())) {
            queryWrapper.eq("disabled", SwitchEnum.OPEN.name().equals(memberSearchVO.getDisabled()) ? 1 : 0);
        }
        queryWrapper.orderByDesc("create_time");
        return memberMapper.selectCount(queryWrapper);
    }

    /**
     * 管理端修改客户信息
     *
     * @param memberService       客户服务实例
     * @param managerMemberEditDTO 修改参数
     * @return 修改后的客户对象
     */
    public Member updateMember(MemberService memberService, ManagerMemberEditDTO managerMemberEditDTO) {
        if (CharSequenceUtil.isNotBlank(managerMemberEditDTO.getNickName())) {
            managerMemberEditDTO.setNickName(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, managerMemberEditDTO.getNickName()));
        }
        boolean passwordChanged = CharSequenceUtil.isNotBlank(managerMemberEditDTO.getPassword());
        if (passwordChanged) {
            managerMemberEditDTO.setPassword(new BCryptPasswordEncoder().encode(managerMemberEditDTO.getPassword()));
        }
        Member member = memberService.getById(managerMemberEditDTO.getId());
        BeanUtil.copyProperties(managerMemberEditDTO, member);
        memberService.updateById(member);
        if (passwordChanged) {
            memberService.revokeMemberTokens(Collections.singletonList(member.getId()));
        }
        return member;
    }

    /**
     * 查询指定客户的指定字段数据
     *
     * @param memberMapper 客户 Mapper
     * @param columns      字段列表
     * @param memberIds    客户ID列表
     * @return 映射结果列表
     */
    public List<Map<String, Object>> listFieldsByMemberIds(MemberMapper memberMapper, String columns, List<String> memberIds) {
        return memberMapper.selectMaps(new QueryWrapper<Member>()
                .select(columns)
                .in(memberIds != null && !memberIds.isEmpty(), "id", memberIds));
    }
}
