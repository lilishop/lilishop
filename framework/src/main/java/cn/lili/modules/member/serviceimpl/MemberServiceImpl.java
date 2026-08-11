package cn.lili.modules.member.serviceimpl;


import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.sensitive.SensitiveWordsFilter;
import cn.lili.common.utils.*;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.aop.annotation.PointLogPoint;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.dto.*;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.entity.enums.QRCodeLoginSessionStatusEnum;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.entity.vo.QRCodeLoginSessionVo;
import cn.lili.modules.member.entity.vo.QRLoginResultVo;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.MemberShareRegisterService;
import cn.lili.modules.member.token.MemberTokenGenerate;
import cn.lili.modules.member.token.StoreTokenGenerate;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import cn.lili.message.LiliMessageTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Lazy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

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
     * 微信授权默认返回的昵称，无法区分用户，需要拼接 openid 后缀
     */
    private static final String WECHAT_DEFAULT_NICKNAME = "微信用户";

    private static final int NICKNAME_SUFFIX_LENGTH = 4;

    /**
     * 客户token
     */
    @Autowired
    private MemberTokenGenerate memberTokenGenerate;
    /**
     * 商家token
     */
    @Autowired
    @Lazy
    private StoreTokenGenerate storeTokenGenerate;
    /**
     * 联合登录
     */
    @Autowired
    @Lazy
    private ConnectService connectService;
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
    @Lazy
    private MemberShareRegisterService memberShareRegisterService;

    @Autowired
    @Lazy
    private MemberGradeService memberGradeService;

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
            if(member != null && !member.getDisabled()){
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
        Member member = this.findMember(username);
        //判断用户是否存在
        if (member == null || !member.getDisabled()) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        //判断密码是否输入正确
        if (!new BCryptPasswordEncoder().matches(password, member.getPassword())) {
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR);
        }
        loginBindUser(member);
        return memberTokenGenerate.createToken(member, false);
    }


    @Override
    public void resetPassword(List<String> ids) {
        String password = new BCryptPasswordEncoder().encode(StringUtils.md5("123456"));
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.in(Member::getId, ids);
        lambdaUpdateWrapper.set(Member::getPassword, password);
        this.update(lambdaUpdateWrapper);
    }

    @Override
    public void updateHaveShop(Boolean haveStore, String storeId, List<String> memberIds) {
        List<Member> members = this.listByIds(memberIds);
        if (members.size() > 0) {
            members.forEach(member -> {
                member.setHaveStore(haveStore);
                if (haveStore) {
                    member.setStoreId(storeId);
                } else {
                    member.setStoreId(null);
                }
            });
            this.updateBatchById(members);
        }
    }

    @Override
    public Token usernameStoreLogin(String username, String password) {

        Member member = this.findMember(username);
        //判断用户是否存在
        if (member == null || !member.getDisabled()) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        //判断密码是否输入正确
        if (!new BCryptPasswordEncoder().matches(password, member.getPassword())) {
            throw new ServiceException(ResultCode.USER_PASSWORD_ERROR);
        }
        //对店铺状态的判定处理
        return checkMemberStore(member);
    }

    @Override
    public Token mobilePhoneStoreLogin(String mobilePhone) {
        Member member = this.findMember(mobilePhone);
        //如果手机号不存在则自动注册用户
        if (member == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }
        loginBindUser(member);
        //对店铺状态的判定处理
        return checkMemberStore(member);
    }

    @Override
    @Transactional
    public Member autoRegister(ConnectAuthUser authUser) {

        if (CharSequenceUtil.isEmpty(authUser.getNickname())) {
            authUser.setNickname(CommonUtil.getSpecialStr("用户"));
        } else if (WECHAT_DEFAULT_NICKNAME.equals(authUser.getNickname().trim())) {
            authUser.setNickname(WECHAT_DEFAULT_NICKNAME + openIdSuffix(authUser.getUuid()));
        }
        if (CharSequenceUtil.isEmpty(authUser.getAvatar())) {
            authUser.setAvatar("https://i.loli.net/2020/11/19/LyN6JF7zZRskdIe.png");
        }
        try {
//            String username = UuidUtils.getUUID();
            Member member = new Member(authUser.getUsername(), UuidUtils.getUUID(), authUser.getAvatar(), authUser.getNickname(),
                    authUser.getGender() != null ? Convert.toInt(authUser.getGender().getCode()) : 0, authUser.getPhone());
            member.setPassword(DEFAULT_PASSWORD);
            // 发送客户注册信息
            registerHandler(member);

            return member;
        } catch (ServiceException e) {
            log.error("自动注册服务抛出异常：", e);
            throw e;
        } catch (Exception e) {
            log.error("自动注册异常：", e);
            throw new ServiceException(ResultCode.USER_AUTO_REGISTER_ERROR);
        }
    }

//    @Override
//    @Transactional
//    public Token autoRegister() {
//        ConnectAuthUser connectAuthUser = this.checkConnectUser();
//        return this.autoRegister(connectAuthUser);
//    }

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
        //如果手机号不存在则自动注册用户
        if (member == null) {
            member = new Member(mobilePhone, UuidUtils.getUUID(), mobilePhone);
            registerHandler(member);
        }
        //判断用户是否有效
        if (member.getDisabled().equals(false) || member.getDeleteFlag().equals(true)) {
            throw new ServiceException(ResultCode.USER_STATUS_ERROR);
        }
        loginBindUser(member);
        return memberTokenGenerate.createToken(member, false);
    }

    /**
     * 注册方法抽象
     *
     * @param member
     */
    @Transactional
    public void registerHandler(Member member) {
        member.setId(SnowFlake.getIdStr());
        // 新客户默认写入客户等级（按经验值匹配，无匹配则取默认等级）
        assignDefaultMemberGradeIfAbsent(member);
        //保存客户
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
        MemberGrade grade = memberGradeService.getCurrentGradeByExperience(
                member.getExperience() == null ? 0L : member.getExperience());
        if (grade != null) {
            member.setGradeId(grade.getId());
        }
    }

    @Override
    public Member editOwn(MemberEditDTO memberEditDTO) {
        //查询客户信息
        Member member = this.findByUsername(Objects.requireNonNull(UserContext.getCurrentUser()).getUsername());
        //传递修改客户信息
        BeanUtil.copyProperties(memberEditDTO, member);
        //修改客户
        this.updateById(member);
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_INFO_EDIT.name();
        //发送订单变更mq消息
        liliMessageTemplate.asyncSend(destination, member, MessageSendCallbackBuilder.commonCallback());
        return member;
    }

    @DemoSite
    @Override
    public Member modifyPass(String oldPassword, String newPassword) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = this.getById(tokenUser.getId());
        //判断旧密码输入是否正确
        if (!new BCryptPasswordEncoder().matches(oldPassword, member.getPassword())) {
            throw new ServiceException(ResultCode.USER_OLD_PASSWORD_ERROR);
        }
        //修改客户密码
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, member.getId());
        lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(newPassword));
        this.update(lambdaUpdateWrapper);
        return member;
    }

    @Override
    public boolean canInitPass() {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = this.getById(tokenUser.getId());
        return member.getPassword().equals(DEFAULT_PASSWORD);

    }

    @Override
    public void initPass(String password) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = this.getById(tokenUser.getId());
        if (member.getPassword().equals(DEFAULT_PASSWORD)) {
            //修改客户密码
            LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.eq(Member::getId, member.getId());
            lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(password));
            this.update(lambdaUpdateWrapper);
        }
        throw new ServiceException(ResultCode.UNINITIALIZED_PASSWORD);

    }

    @Override
    public void cancellation() {

        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = this.getById(tokenUser.getId());
        //删除联合登录
        connectService.deleteByMemberId(member.getId());
        //混淆用户信息
        this.confusionMember(member);
    }

    @Override
    @Transactional
    public Token register(String userName, String password, String mobilePhone) {
        return register(userName, password, mobilePhone, null);
    }

    @Override
    @Transactional
    public Token register(String userName, String password, String mobilePhone, String shareCode) {
        //检测客户信息
        checkMember(userName, mobilePhone);
        //设置客户信息
        Member member = new Member(userName, new BCryptPasswordEncoder().encode(password), mobilePhone);
        //注册成功后用户自动登录
        registerHandler(member);
        // 绑定分享注册来源（仅当分享码有效时生效）
        memberShareRegisterService.bindRegisterSource(member.getId(), shareCode);
        return memberTokenGenerate.createToken(member, false);
    }

    @Override
    public boolean changeMobile(String mobile) {
        AuthUser tokenUser = Objects.requireNonNull(UserContext.getCurrentUser());
        Member member = this.findByUsername(tokenUser.getUsername());

        //判断是否用户登录并且客户ID为当前登录客户ID
        if (!Objects.equals(tokenUser.getId(), member.getId())) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        //修改客户手机号
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, member.getId());
        lambdaUpdateWrapper.set(Member::getMobile, mobile);
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public boolean changeMobile(String memberId, String mobile) {
        //修改客户手机号
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, memberId);
        lambdaUpdateWrapper.set(Member::getMobile, mobile);
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public boolean resetByMobile(String uuid, String password) {
        String phone = cache.get(CachePrefix.FIND_MOBILE + uuid).toString();
        //根据手机号获取客户判定是否存在此客户
        if (phone != null) {
            //修改密码
            LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.eq(Member::getMobile, phone);
            lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(password));
            cache.remove(CachePrefix.FIND_MOBILE + uuid);
            return this.update(lambdaUpdateWrapper);
        } else {
            throw new ServiceException(ResultCode.USER_PHONE_NOT_EXIST);
        }

    }

    @Override
    @Transactional
    public Member addMember(MemberAddDTO memberAddDTO) {

        //检测客户信息
        checkMember(memberAddDTO.getUsername(), memberAddDTO.getMobile());

        //添加客户
        Member member = new Member(memberAddDTO.getUsername(), new BCryptPasswordEncoder().encode(memberAddDTO.getPassword()),
                memberAddDTO.getMobile());
        registerHandler(member);
        return member;
    }

    @Override
    public Member updateMember(ManagerMemberEditDTO managerMemberEditDTO) {
        //过滤客户昵称敏感词
        if (CharSequenceUtil.isNotBlank(managerMemberEditDTO.getNickName())) {
            managerMemberEditDTO.setNickName(SensitiveWordsFilter.filter(managerMemberEditDTO.getNickName()));
        }
        //如果密码不为空则加密密码
        if (CharSequenceUtil.isNotBlank(managerMemberEditDTO.getPassword())) {
            managerMemberEditDTO.setPassword(new BCryptPasswordEncoder().encode(managerMemberEditDTO.getPassword()));
        }
        //查询客户信息
        Member member = this.getById(managerMemberEditDTO.getId());
        //传递修改客户信息
        BeanUtil.copyProperties(managerMemberEditDTO, member);
        this.updateById(member);
        return member;
    }

    @Override
    public IPage<MemberVO> getMemberPage(MemberSearchVO memberSearchVO, PageVO page) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        //用户名查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getUsername()), "m.username", memberSearchVO.getUsername());
        //用户名查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getNickName()), "m.nick_name", memberSearchVO.getNickName());
        //按照电话号码查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getMobile()), "m.mobile", memberSearchVO.getMobile());
        //按会员等级筛选
        queryWrapper.eq(CharSequenceUtil.isNotBlank(memberSearchVO.getGradeId()), "m.grade_id", memberSearchVO.getGradeId());
        //按照积分范围查询
        queryWrapper.ge(memberSearchVO.getMinPoint() != null, "m.point", memberSearchVO.getMinPoint());
        queryWrapper.le(memberSearchVO.getMaxPoint() != null, "m.point", memberSearchVO.getMaxPoint());
        //按照ID查询
        queryWrapper.eq(CharSequenceUtil.isNotBlank(memberSearchVO.getId()), "m.id", memberSearchVO.getId());
        //按照客户状态查询
        if (CharSequenceUtil.isNotBlank(memberSearchVO.getDisabled())) {
            queryWrapper.eq("m.disabled", SwitchEnum.OPEN.name().equals(memberSearchVO.getDisabled()) ? 1 : 0);
        }
        //按照客户分组查询
        if (CharSequenceUtil.isNotBlank(memberSearchVO.getGroupId())) {
            queryWrapper.inSql("m.id", "SELECT member_id FROM li_member_group_user WHERE group_id = '" + memberSearchVO.getGroupId() + "' AND delete_flag = false");
        }
        queryWrapper.orderByDesc("m.create_time");
        return this.baseMapper.pageByMemberVO(PageUtil.initPage(page), queryWrapper);
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
        //获取当前客户信息
        Member member = this.getById(memberId);
        if (member != null) {
            //积分变动后的客户积分
            long currentPoint;
            //客户总获得积分
            long totalPoint = member.getTotalPoint();
            //如果增加积分
            if (type.equals(PointTypeEnum.INCREASE.name())) {
                currentPoint = member.getPoint() + point;
                //如果是增加积分 需要增加总获得积分
                totalPoint = totalPoint + point;
            }
            //否则扣除积分
            else {
                currentPoint = member.getPoint() - point < 0 ? 0 : member.getPoint() - point;
            }
            member.setPoint(currentPoint);
            member.setTotalPoint(totalPoint);
            boolean result = this.updateById(member);
            if (result) {
                //发送客户消息
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

        //如果是禁用
        if (Boolean.FALSE.equals(status)) {
            disableMemberLogout(memberIds);
        }
        return this.update(updateWrapper);
    }

//    /**
//     * 检测是否可以绑定第三方联合登陆
//     * 返回null原因
//     * 包含原因1：redis中已经没有联合登陆信息  2：已绑定其他账号
//     *
//     * @return 返回对象则代表可以进行绑定第三方客户，返回null则表示联合登陆无法继续
//     */
//    private ConnectAuthUser checkConnectUser() {
//        //获取cookie存储的信息
//        String uuid = CookieUtil.getCookie(ConnectService.CONNECT_COOKIE, ThreadContextHolder.getHttpRequest());
//        String connectType = CookieUtil.getCookie(ConnectService.CONNECT_TYPE, ThreadContextHolder.getHttpRequest());
//
//        //如果联合登陆存储了信息
//        if (CharSequenceUtil.isNotEmpty(uuid) && CharSequenceUtil.isNotEmpty(connectType)) {
//            //枚举 联合登陆类型获取
//            ConnectAuthEnum authInterface = ConnectAuthEnum.valueOf(connectType);
//
//            ConnectAuthUser connectAuthUser = getConnectAuthUser(uuid, connectType);
//            if (connectAuthUser == null) {
//                throw new ServiceException(ResultCode.USER_OVERDUE_CONNECT_ERROR);
//            }
//            //检测是否已经绑定过用户
//            Connect connect = connectService.queryConnect(
//                    ConnectQueryDTO.builder().unionType(connectType).unionId(connectAuthUser.getUuid()).build()
//            );
//            //没有关联则返回true，表示可以继续绑定
//            if (connect == null) {
//                connectAuthUser.setConnectEnum(authInterface);
//                return connectAuthUser;
//            } else {
//                throw new ServiceException(ResultCode.USER_CONNECT_BANDING_ERROR);
//            }
//        } else {
//            throw new ServiceException(ResultCode.USER_CONNECT_NOT_EXIST_ERROR);
//        }
//    }

    @Override
    public long getMemberNum(MemberSearchVO memberSearchVO) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        //用户名查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getUsername()), "username", memberSearchVO.getUsername());
        //按照电话号码查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getMobile()), "mobile", memberSearchVO.getMobile());
        //按照积分范围查询
        queryWrapper.ge(memberSearchVO.getMinPoint() != null, "point", memberSearchVO.getMinPoint());
        queryWrapper.le(memberSearchVO.getMaxPoint() != null, "point", memberSearchVO.getMaxPoint());
        //按照状态查询
        if (CharSequenceUtil.isNotBlank(memberSearchVO.getDisabled())) {
            queryWrapper.eq("disabled", SwitchEnum.OPEN.name().equals(memberSearchVO.getDisabled()) ? 1 : 0);
        }
        queryWrapper.orderByDesc("create_time");
        return this.count(queryWrapper);
    }

    /**
     * 获取指定客户数据
     *
     * @param columns   指定获取的列
     * @param memberIds 客户ids
     * @return 指定客户数据
     */
    @Override
    public List<Map<String, Object>> listFieldsByMemberIds(String columns, List<String> memberIds) {
        return this.listMaps(new QueryWrapper<Member>()
                .select(columns)
                .in(memberIds != null && !memberIds.isEmpty(), "id", memberIds));
    }

    /**
     * 登出
     */
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

    /**
     * 禁用客户token删除
     *
     * @param memberIds 客户id
     */
    public void disableMemberLogout(List<String> memberIds) {
        if (memberIds != null) {
            memberIds.forEach(memberId -> {
                cache.vagueDel(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER, memberId));
                cache.vagueDel(CachePrefix.REFRESH_TOKEN.getPrefix(UserEnums.MEMBER, memberId));
            });
        }
    }

    /**
     * 获取所有客户的手机号
     *
     * @return 所有客户的手机号
     */
    @Override
    public List<String> getAllMemberMobile() {
        return this.baseMapper.getAllMemberMobile();
    }

    /**
     * 更新客户登录时间为最新时间
     *
     * @param memberId 客户id
     * @return 是否更新成功
     */
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
        QRCodeLoginSessionVo session = new QRCodeLoginSessionVo();
        session.setStatus(QRCodeLoginSessionStatusEnum.WAIT_SCANNING.getCode());
        //过期时间，20s
        Long duration = 20 * 1000L;
        session.setDuration(duration);
        String token = CachePrefix.QR_CODE_LOGIN_SESSION.name() + SnowFlake.getIdStr();
        session.setToken(token);
        cache.put(token, session, duration, TimeUnit.MILLISECONDS);
        return session;
    }

    @Override
    public Object appScanner(String token) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        QRCodeLoginSessionVo session = (QRCodeLoginSessionVo) cache.get(token);
        if (session == null) {
            return QRCodeLoginSessionStatusEnum.NO_EXIST.getCode();
        }
        session.setStatus(QRCodeLoginSessionStatusEnum.SCANNING.getCode());
        cache.put(token, session, session.getDuration(), TimeUnit.MILLISECONDS);
        return QRCodeLoginSessionStatusEnum.SCANNING.getCode();
    }

    @Override
    public boolean appSConfirm(String token, Integer code) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        QRCodeLoginSessionVo session = (QRCodeLoginSessionVo) cache.get(token);
        if (session == null) {
            return false;
        }
        if (code == 1) {
            //同意
            session.setStatus(QRCodeLoginSessionStatusEnum.VERIFIED.getCode());
            session.setUserId(Long.parseLong(tokenUser.getId()));
        } else {
            //拒绝
            session.setStatus(QRCodeLoginSessionStatusEnum.CANCELED.getCode());
        }
        cache.put(token, session, session.getDuration(), TimeUnit.MILLISECONDS);
        return true;
    }

    @Override
    public QRLoginResultVo loginWithSession(String sessionToken) {
        QRLoginResultVo result = new QRLoginResultVo();
        result.setStatus(QRCodeLoginSessionStatusEnum.NO_EXIST.getCode());
        QRCodeLoginSessionVo session = (QRCodeLoginSessionVo) cache.get(sessionToken);
        if (session == null) {
            return result;
        }
        result.setStatus(session.getStatus());
        if (QRCodeLoginSessionStatusEnum.VERIFIED.getCode().equals(session.getStatus())) {
            //生成token
            Member member = this.getById(session.getUserId());
            if (member == null) {
                throw new ServiceException(ResultCode.USER_NOT_EXIST);
            } else {
                //生成token
                Token token = memberTokenGenerate.createToken(member, false);
                result.setToken(token);
                cache.vagueDel(sessionToken);
            }

        }
        return result;
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

    /**
     * 传递手机号或者用户名
     *
     * @param userName 手机号或者用户名
     * @return 客户信息
     */
    private Member findMember(String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("username", userName).or().eq("mobile", userName);
        return this.getOne(queryWrapper, false);
    }

    /**
     * 根据手机号获取客户
     *
     * @param mobilePhone 手机号
     * @return 客户
     */
    private Long findMember(String mobilePhone, String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("mobile", mobilePhone)
                .or().eq("username", userName);
        return this.baseMapper.selectCount(queryWrapper);
    }

    /**
     * 取 openid 后四位作为昵称后缀，openid 缺失时使用随机数兜底
     *
     * @param openId 微信openid
     * @return 昵称后缀
     */
    private String openIdSuffix(String openId) {
        if (CharSequenceUtil.isEmpty(openId)) {
            return CommonUtil.getRandomNum();
        }
        return openId.length() > NICKNAME_SUFFIX_LENGTH
                ? openId.substring(openId.length() - NICKNAME_SUFFIX_LENGTH)
                : openId;
    }

    /**
     * 混淆之前的客户信息
     *
     * @param member
     */
    private void confusionMember(Member member) {
        member.setUsername(UuidUtils.getUUID());
        member.setMobile(UuidUtils.getUUID() + member.getMobile());
        member.setNickName("用户已注销");
        member.setDisabled(false);
        this.updateById(member);
    }

    /**
     * 获取cookie中的联合登录对象
     *
     * @param uuid uuid
     * @param type 状态
     * @return cookie中的联合登录对象
     */
    private ConnectAuthUser getConnectAuthUser(String uuid, String type) {
        Object context = cache.get(ConnectService.cacheKey(type, uuid));
        if (context != null) {
            return (ConnectAuthUser) context;
        }
        return null;
    }

    /**
     * 成功登录，则检测cookie中的信息，进行客户绑定
     *
     * @param member 客户
     */
    private void loginBindUser(Member member) {
        //获取cookie存储的信息
        String uuid = CookieUtil.getCookie(ConnectService.CONNECT_COOKIE, ThreadContextHolder.getHttpRequest());
        String connectType = CookieUtil.getCookie(ConnectService.CONNECT_TYPE, ThreadContextHolder.getHttpRequest());
        //如果联合登陆存储了信息
        if (CharSequenceUtil.isNotEmpty(uuid) && CharSequenceUtil.isNotEmpty(connectType)) {
            try {
                //获取信息
                ConnectAuthUser connectAuthUser = getConnectAuthUser(uuid, connectType);
                if (connectAuthUser == null) {
                    return;
                }
                Connect connect = connectService.queryConnect(
                        ConnectQueryDTO.builder().unionId(connectAuthUser.getUuid()).unionType(connectType).build()
                );
                if (connect == null) {
                    connect = new Connect(member.getId(), connectAuthUser.getUuid(), connectType);
                    connectService.save(connect);
                }
            } catch (ServiceException e) {
                throw e;
            } catch (Exception e) {
                log.error("绑定第三方联合登陆失败：", e);
            } finally {
                //联合登陆成功与否，都清除掉cookie中的信息
                CookieUtil.delCookie(ConnectService.CONNECT_COOKIE, ThreadContextHolder.getHttpResponse());
                CookieUtil.delCookie(ConnectService.CONNECT_TYPE, ThreadContextHolder.getHttpResponse());
            }
        }

    }

    /**
     * 检测客户
     *
     * @param userName    客户名称
     * @param mobilePhone 手机号
     */
    private void checkMember(String userName, String mobilePhone) {
        //判断手机号是否存在
        if (findMember(mobilePhone, userName) > 0) {
            throw new ServiceException(ResultCode.USER_EXIST);
        }
    }

}
