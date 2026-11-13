package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.support.LoginFailLimitSupport;
import cn.lili.common.security.token.Token;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.common.sensitive.SensitiveWordsHelper;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.message.LiliMessageTemplate;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.member.aop.annotation.PointLogPoint;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.dto.*;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.entity.vo.QRCodeLoginSessionVo;
import cn.lili.modules.member.entity.vo.QRLoginResultVo;
import cn.lili.modules.member.event.MemberRegisterEvent;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.support.MemberConnectSupport;
import cn.lili.modules.member.support.MemberManageSupport;
import cn.lili.modules.member.support.MemberQrCodeLoginSupport;
import cn.lili.modules.member.support.MemberSecuritySupport;
import cn.lili.modules.member.token.MemberTokenGenerate;
import cn.lili.modules.member.token.StoreTokenGenerate;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 客户接口业务层实现
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
@Slf4j
@Service
public class MemberServiceImpl extends ServiceImpl<MemberMapper, Member> implements MemberService {

    /**
     * 客户token
     */
    @Autowired
    private MemberTokenGenerate memberTokenGenerate;
    /**
     * 商家token
     */
    @Autowired
    private StoreTokenGenerate storeTokenGenerate;
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * RocketMQ 配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;

    @Autowired
    private LiliMessageTemplate liliMessageTemplate;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    @Autowired
    private LoginFailLimitSupport loginFailLimitSupport;

    @Autowired
    private MemberGradeService memberGradeService;

    @Autowired(required = false)
    private MemberQrCodeLoginSupport memberQrCodeLoginSupport;

    @Autowired(required = false)
    private MemberConnectSupport memberConnectSupport;

    @Autowired(required = false)
    private MemberManageSupport memberManageSupport;

    @Autowired(required = false)
    private MemberSecuritySupport memberSecuritySupport;

    @Override
    public Member findByUsername(String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("username", userName);
        return this.baseMapper.selectOne(queryWrapper);
    }

    @Override
    public Member getUserInfo() {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser != null) {
            Member member = this.findByUsername(tokenUser.getUsername());
            if (member != null && !member.getDisabled()) {
                throw new ServiceException(ResultCode.USER_STATUS_ERROR);
            }
            return member;
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
    }

    @Override
    public Member findByMobile(String mobile) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("mobile", mobile);
        return this.baseMapper.selectOne(queryWrapper);
    }

    @Override
    public boolean findByMobile(String uuid, String mobile) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("mobile", mobile);
        Member member = this.baseMapper.selectOne(queryWrapper);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_PHONE);
        }
        cache.put(CachePrefix.FIND_MOBILE + uuid, mobile, 300L);
        return true;
    }

    @Override
    public Token usernameLogin(String username, String password) {
        loginFailLimitSupport.checkLoginLock(UserEnums.MEMBER, username);
        Member member = this.findMember(username);
        // 判断用户是否存在
        if (member == null || !member.getDisabled()) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        // 判断密码是否输入正确
        if (!new BCryptPasswordEncoder().matches(password, member.getPassword())) {
            loginFailLimitSupport.recordPasswordFail(UserEnums.MEMBER, username);
        }
        loginFailLimitSupport.clearLoginFail(UserEnums.MEMBER, username);
        loginBindUser(member);
        return memberTokenGenerate.createToken(member, false);
    }

    @Override
    public void resetPassword(List<String> ids) {
        getMemberSecuritySupport().resetPassword(this, ids);
    }

    @Override
    public void updateHaveShop(Boolean haveStore, String storeId, List<String> memberIds) {
        getMemberManageSupport().updateHaveShop(this, haveStore, storeId, memberIds);
    }

    @Override
    public Token usernameStoreLogin(String username, String password) {
        loginFailLimitSupport.checkLoginLock(UserEnums.STORE, username);
        Member member = this.findMember(username);
        // 判断用户是否存在
        if (member == null || !member.getDisabled()) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        // 判断密码是否输入正确
        if (!new BCryptPasswordEncoder().matches(password, member.getPassword())) {
            loginFailLimitSupport.recordPasswordFail(UserEnums.STORE, username);
        }
        loginFailLimitSupport.clearLoginFail(UserEnums.STORE, username);
        // 对店铺状态的判定处理
        return checkMemberStore(member);
    }

    @Override
    public Token mobilePhoneStoreLogin(String mobilePhone) {
        Member member = this.findMember(mobilePhone);
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        loginBindUser(member);
        return checkMemberStore(member);
    }

    @Override
    @Transactional
    public Member autoRegister(ConnectAuthUser authUser) {
        return getMemberConnectSupport().autoRegister(authUser, this::registerHandler);
    }

    @Override
    public Token refreshToken(String refreshToken) {
        return memberTokenGenerate.refreshToken(refreshToken);
    }

    @Override
    public Token refreshStoreToken(String refreshToken) {
        return storeTokenGenerate.refreshToken(refreshToken);
    }

    @Override
    @Transactional
    public Token mobilePhoneLogin(String mobilePhone) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("mobile", mobilePhone);
        Member member = this.baseMapper.selectOne(queryWrapper, false);
        // 如果手机号不存在则自动注册用户
        if (member == null) {
            member = new Member(mobilePhone, SnowFlake.getIdStr(), mobilePhone);
            registerHandler(member);
        }
        // 判断用户是否有效
        if (Boolean.FALSE.equals(member.getDisabled()) || Boolean.TRUE.equals(member.getDeleteFlag())) {
            throw new ServiceException(ResultCode.USER_STATUS_ERROR);
        }
        loginBindUser(member);
        return memberTokenGenerate.createToken(member, false);
    }

    @Transactional
    public void registerHandler(Member member) {
        member.setId(SnowFlake.getIdStr());
        // 新客户默认写入客户等级（按经验值匹配，无匹配则取默认等级）
        assignDefaultMemberGradeIfAbsent(member);
        // 保存客户
        this.save(member);
        UserContext.settingInviter(member.getId(), cache);
        // 发送客户注册信息
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("new member register", rocketmqCustomProperties.getMemberTopic(),
                MemberTagsEnum.MEMBER_REGISTER.name(), member));
    }

    @Override
    public void assignDefaultMemberGradeIfAbsent(Member member) {
        if (member == null || CharSequenceUtil.isNotBlank(member.getGradeId())) {
            return;
        }
        if (memberGradeService != null) {
            MemberGrade grade = memberGradeService.getCurrentGradeByExperience(
                    member.getExperience() == null ? 0L : member.getExperience());
            if (grade != null) {
                member.setGradeId(grade.getId());
            }
        }
    }

    @Override
    public Member editOwn(MemberEditDTO memberEditDTO) {
        // 过滤客户昵称敏感词
        if (CharSequenceUtil.isNotBlank(memberEditDTO.getNickName())) {
            memberEditDTO.setNickName(SensitiveWordsHelper.filter(SensitiveFilterScene.MEMBER, memberEditDTO.getNickName()));
        }
        // 查询客户信息
        Member member = this.findByUsername(Objects.requireNonNull(UserContext.getCurrentUser()).getUsername());
        // 传递修改客户信息
        BeanUtil.copyProperties(memberEditDTO, member);
        // 修改客户
        this.updateById(member);
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_INFO_EDIT.name();
        // 发送订单变更mq消息
        liliMessageTemplate.asyncSend(destination, member, MessageSendCallbackBuilder.commonCallback());
        return member;
    }

    @DemoSite
    @Override
    public Member modifyPass(String oldPassword, String newPassword) {
        return getMemberSecuritySupport().modifyPass(this, oldPassword, newPassword);
    }

    @Override
    public boolean canInitPass() {
        return getMemberSecuritySupport().canInitPass(this);
    }

    @Override
    public void initPass(String password) {
        getMemberSecuritySupport().initPass(this, password);
    }

    @Override
    public void cancellation() {
        getMemberSecuritySupport().cancellation(this);
    }

    @Override
    @Transactional
    public Token register(String userName, String password, String mobilePhone) {
        return register(userName, password, mobilePhone, null);
    }

    @Override
    @Transactional
    public Token register(String userName, String password, String mobilePhone, String shareCode) {
        // 检测客户信息
        checkMember(userName, mobilePhone);
        // 设置客户信息
        Member member = new Member(userName, new BCryptPasswordEncoder().encode(password), mobilePhone);
        // 注册成功后用户自动登录
        registerHandler(member);
        // 发布客户注册事件，解耦绑定分享邀请来源等后续处理逻辑
        applicationEventPublisher.publishEvent(new MemberRegisterEvent(this, member, shareCode));
        return memberTokenGenerate.createToken(member, false);
    }

    @Override
    public boolean changeMobile(String mobile) {
        return getMemberSecuritySupport().changeMobile(this, mobile);
    }

    @Override
    public boolean changeMobile(String memberId, String mobile) {
        return getMemberSecuritySupport().changeMobile(this, memberId, mobile);
    }

    @Override
    public boolean resetByMobile(String uuid, String password) {
        return getMemberSecuritySupport().resetByMobile(this, uuid, password);
    }

    @Override
    @Transactional
    public Member addMember(MemberAddDTO memberAddDTO) {
        checkMember(memberAddDTO.getUsername(), memberAddDTO.getMobile());
        Member member = new Member(memberAddDTO.getUsername(), new BCryptPasswordEncoder().encode(memberAddDTO.getPassword()),
                memberAddDTO.getMobile());
        registerHandler(member);
        return member;
    }

    @Override
    public Member updateMember(ManagerMemberEditDTO managerMemberEditDTO) {
        return getMemberManageSupport().updateMember(this, managerMemberEditDTO);
    }

    @Override
    public IPage<MemberVO> getMemberPage(MemberSearchVO memberSearchVO, PageVO page) {
        return getMemberManageSupport().getMemberPage(this.baseMapper, memberSearchVO, page);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateMemberPoint(Long point, String type, String memberId, String content) {
        return updateMemberPoint(point, type, memberId, content, cn.lili.modules.member.entity.enums.PointSourceEnum.UNKNOWN.name());
    }

    @Override
    @PointLogPoint
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateMemberPoint(Long point, String type, String memberId, String content, String pointSource) {
        Member member = this.getById(memberId);
        if (member != null) {
            long currentPoint;
            long totalPoint = member.getTotalPoint();
            if (type.equals(PointTypeEnum.INCREASE.name())) {
                currentPoint = member.getPoint() + point;
                totalPoint = totalPoint + point;
            } else {
                currentPoint = member.getPoint() - point < 0 ? 0 : member.getPoint() - point;
            }
            member.setPoint(currentPoint);
            member.setTotalPoint(totalPoint);
            boolean result = this.updateById(member);
            if (result) {
                MemberPointMessage memberPointMessage = new MemberPointMessage();
                memberPointMessage.setPoint(point);
                memberPointMessage.setType(type);
                memberPointMessage.setMemberId(memberId);
                applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("update member point",
                        rocketmqCustomProperties.getMemberTopic(), MemberTagsEnum.MEMBER_POINT_CHANGE.name(), memberPointMessage));
                return true;
            }
            return false;
        }
        throw new ServiceException(ResultCode.USER_NOT_EXIST);
    }

    @Override
    public Boolean updateMemberStatus(List<String> memberIds, Boolean status) {
        UpdateWrapper<Member> updateWrapper = Wrappers.update();
        updateWrapper.set("disabled", status);
        updateWrapper.in("id", memberIds);
        if (Boolean.FALSE.equals(status)) {
            disableMemberLogout(memberIds);
        }
        return this.update(updateWrapper);
    }

    @Override
    public long getMemberNum(MemberSearchVO memberSearchVO) {
        return getMemberManageSupport().getMemberNum(this.baseMapper, memberSearchVO);
    }

    @Override
    public List<Map<String, Object>> listFieldsByMemberIds(String columns, List<String> memberIds) {
        return getMemberManageSupport().listFieldsByMemberIds(this.baseMapper, columns, memberIds);
    }

    @Override
    public void logout(UserEnums userEnums) {
        String currentUserToken = UserContext.getCurrentUserToken();
        AuthUser authUser = UserContext.getAuthUser(currentUserToken);
        if (CharSequenceUtil.isNotEmpty(currentUserToken)) {
            cache.remove(CachePrefix.ACCESS_TOKEN.getPrefix(userEnums, authUser.getId()) + currentUserToken);
            cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(userEnums, authUser.getId()));
        }
    }

    @Override
    public void logout(String userId) {
        cache.vagueDel(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MANAGER, userId));
        cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MANAGER, userId));
    }

    @Override
    public void revokeMemberTokens(List<String> memberIds) {
        if (memberIds != null && !memberIds.isEmpty()) {
            memberIds.forEach(memberId -> {
                cache.vagueDel(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER, memberId));
                cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, memberId));
                cache.vagueDel(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.STORE, memberId));
                cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.STORE, memberId));
            });
        }
    }

    public void disableMemberLogout(List<String> memberIds) {
        revokeMemberTokens(memberIds);
    }

    @Override
    public boolean updateMemberLoginTime(String memberId) {
        LambdaUpdateWrapper<Member> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Member::getId, memberId);
        updateWrapper.set(Member::getLastLoginDate, new Date());
        return this.update(updateWrapper);
    }

    @Override
    public MemberVO getMember(String id) {
        return this.baseMapper.getMemberVOById(id);
    }

    @Override
    public QRCodeLoginSessionVo createPcSession() {
        return getMemberQrCodeLoginSupport().createPcSession();
    }

    @Override
    public Object appScanner(String token) {
        return getMemberQrCodeLoginSupport().appScanner(token);
    }

    @Override
    public boolean appSConfirm(String token, Integer code) {
        return getMemberQrCodeLoginSupport().appSConfirm(token, code);
    }

    @Override
    public QRLoginResultVo loginWithSession(String sessionToken) {
        return getMemberQrCodeLoginSupport().loginWithSession(this, sessionToken);
    }

    private Token checkMemberStore(Member member) {
        if (Boolean.TRUE.equals(member.getHaveStore())) {
            Store store = storeService.getById(member.getStoreId());
            if (!store.getStoreDisable().equals(StoreStatusEnum.OPEN.name())) {
                throw new ServiceException(ResultCode.STORE_CLOSE_ERROR);
            }
        } else {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        return storeTokenGenerate.createToken(member, false);
    }

    private Member findMember(String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("username", userName).or().eq("mobile", userName);
        return this.getOne(queryWrapper, false);
    }

    private Long findMember(String mobilePhone, String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("mobile", mobilePhone)
                .or().eq("username", userName);
        return this.baseMapper.selectCount(queryWrapper);
    }

    private void loginBindUser(Member member) {
        getMemberConnectSupport().loginBindUser(member);
    }

    private void checkMember(String userName, String mobilePhone) {
        if (findMember(mobilePhone, userName) > 0) {
            throw new ServiceException(ResultCode.USER_EXIST);
        }
    }

    private MemberQrCodeLoginSupport getMemberQrCodeLoginSupport() {
        if (this.memberQrCodeLoginSupport == null) {
            this.memberQrCodeLoginSupport = new MemberQrCodeLoginSupport(cache, memberTokenGenerate);
        }
        return this.memberQrCodeLoginSupport;
    }

    private MemberConnectSupport getMemberConnectSupport() {
        if (this.memberConnectSupport == null) {
            this.memberConnectSupport = new MemberConnectSupport();
        }
        return this.memberConnectSupport;
    }

    private MemberManageSupport getMemberManageSupport() {
        if (this.memberManageSupport == null) {
            this.memberManageSupport = new MemberManageSupport();
        }
        return this.memberManageSupport;
    }

    private MemberSecuritySupport getMemberSecuritySupport() {
        if (this.memberSecuritySupport == null) {
            this.memberSecuritySupport = new MemberSecuritySupport();
        }
        return this.memberSecuritySupport;
    }

    public void setMemberQrCodeLoginSupport(MemberQrCodeLoginSupport memberQrCodeLoginSupport) {
        this.memberQrCodeLoginSupport = memberQrCodeLoginSupport;
    }

    public void setMemberConnectSupport(MemberConnectSupport memberConnectSupport) {
        this.memberConnectSupport = memberConnectSupport;
    }

    public void setMemberManageSupport(MemberManageSupport memberManageSupport) {
        this.memberManageSupport = memberManageSupport;
    }

    public void setMemberSecuritySupport(MemberSecuritySupport memberSecuritySupport) {
        this.memberSecuritySupport = memberSecuritySupport;
    }
}
