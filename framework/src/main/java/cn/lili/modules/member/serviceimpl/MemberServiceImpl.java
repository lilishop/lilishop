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
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CookieUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.UuidUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.connect.config.ConnectAuthEnum;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.aop.annotation.PointLogPoint;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.*;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.entity.enums.QRCodeLoginSessionStatusEnum;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.entity.vo.QRCodeLoginSessionVo;
import cn.lili.modules.member.entity.vo.QRLoginResultVo;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.token.MemberTokenGenerate;
import cn.lili.modules.member.token.StoreTokenGenerate;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.tags.MemberTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

/**
 * 会员接口业务层实现
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
@Service
public class MemberServiceImpl extends ServiceImpl<MemberMapper, Member> implements MemberService {

    /**
     * 会员token
     */
    @Autowired
    private MemberTokenGenerate memberTokenGenerate;
    /**
     * 商家token
     */
    @Autowired
    private StoreTokenGenerate storeTokenGenerate;
    /**
     * 联合登录
     */
    @Autowired
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
    private ApplicationEventPublisher applicationEventPublisher;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

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
            return this.findByUsername(tokenUser.getUsername());
        }
        throw new ServiceException(ResultCode.USER_NOT_LOGIN);
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
     * @return 会员信息
     */
    private Member findMember(String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("username", userName).or().eq("mobile", userName);
        return this.getOne(queryWrapper);
    }

    @Override
    @Transactional
    public Token autoRegister(ConnectAuthUser authUser) {

        if (CharSequenceUtil.isEmpty(authUser.getNickname())) {
            authUser.setNickname("临时昵称");
        }
        if (CharSequenceUtil.isEmpty(authUser.getAvatar())) {
            authUser.setAvatar("https://i.loli.net/2020/11/19/LyN6JF7zZRskdIe.png");
        }
        try {
            String username = UuidUtils.getUUID();
            Member member = new Member(username, UuidUtils.getUUID(), authUser.getAvatar(), authUser.getNickname(),
                    authUser.getGender() != null ? Convert.toInt(authUser.getGender().getCode()) : 0);
            registerHandler(member);
            member.setPassword(DEFAULT_PASSWORD);
            //绑定登录方式
            loginBindUser(member, authUser.getUuid(), authUser.getSource());
            return memberTokenGenerate.createToken(member, false);
        } catch (ServiceException e) {
            log.error("自动注册服务抛出异常：", e);
            throw e;
        } catch (Exception e) {
            log.error("自动注册异常：", e);
            throw new ServiceException(ResultCode.USER_AUTO_REGISTER_ERROR);
        }
    }

    @Override
    @Transactional
    public Token autoRegister() {
        ConnectAuthUser connectAuthUser = this.checkConnectUser();
        return this.autoRegister(connectAuthUser);
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
        Member member = this.baseMapper.selectOne(queryWrapper);
        //如果手机号不存在则自动注册用户
        if (member == null) {
            member = new Member(mobilePhone, UuidUtils.getUUID(), mobilePhone);
            registerHandler(member);
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
        //保存会员
        this.save(member);
        // 发送会员注册信息
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("new member register", rocketmqCustomProperties.getMemberTopic(), MemberTagsEnum.MEMBER_REGISTER.name(), member));
    }

    @Override
    public Member editOwn(MemberEditDTO memberEditDTO) {
        //查询会员信息
        Member member = this.findByUsername(Objects.requireNonNull(UserContext.getCurrentUser()).getUsername());
        //传递修改会员信息
        BeanUtil.copyProperties(memberEditDTO, member);
        //修改会员
        this.updateById(member);
        return member;
    }

    @Override
    @DemoSite
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
        //修改会员密码
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
            //修改会员密码
            LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
            lambdaUpdateWrapper.eq(Member::getId, member.getId());
            lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(password));
            this.update(lambdaUpdateWrapper);
        }
        throw new ServiceException(ResultCode.UNINITIALIZED_PASSWORD);

    }

    @Override
    public void cancellation(String password) {

        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        Member member = this.getById(tokenUser.getId());
        if (member.getPassword().equals(new BCryptPasswordEncoder().encode(password))) {
            //删除联合登录
            connectService.deleteByMemberId(member.getId());
            //混淆用户信息
            this.confusionMember(member);
        }
    }

    /**
     * 混淆之前的会员信息
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

    @Override
    @Transactional
    public Token register(String userName, String password, String mobilePhone) {
        //检测会员信息
        checkMember(userName, mobilePhone);
        //设置会员信息
        Member member = new Member(userName, new BCryptPasswordEncoder().encode(password), mobilePhone);
        //注册成功后用户自动登录
        registerHandler(member);
        return memberTokenGenerate.createToken(member, false);
    }

    @Override
    public boolean changeMobile(String mobile) {
        AuthUser tokenUser = Objects.requireNonNull(UserContext.getCurrentUser());
        Member member = this.findByUsername(tokenUser.getUsername());

        //判断是否用户登录并且会员ID为当前登录会员ID
        if (!Objects.equals(tokenUser.getId(), member.getId())) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        //修改会员手机号
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, member.getId());
        lambdaUpdateWrapper.set(Member::getMobile, mobile);
        return this.update(lambdaUpdateWrapper);
    }

    @Override
    public boolean resetByMobile(String uuid, String password) {
        String phone = cache.get(CachePrefix.FIND_MOBILE + uuid).toString();
        //根据手机号获取会员判定是否存在此会员
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

        //检测会员信息
        checkMember(memberAddDTO.getUsername(), memberAddDTO.getMobile());

        //添加会员
        Member member = new Member(memberAddDTO.getUsername(), new BCryptPasswordEncoder().encode(memberAddDTO.getPassword()), memberAddDTO.getMobile());
        registerHandler(member);
        return member;
    }

    @Override
    public Member updateMember(ManagerMemberEditDTO managerMemberEditDTO) {
        //过滤会员昵称敏感词
        if (CharSequenceUtil.isNotBlank(managerMemberEditDTO.getNickName())) {
            managerMemberEditDTO.setNickName(SensitiveWordsFilter.filter(managerMemberEditDTO.getNickName()));
        }
        //如果密码不为空则加密密码
        if (CharSequenceUtil.isNotBlank(managerMemberEditDTO.getPassword())) {
            managerMemberEditDTO.setPassword(new BCryptPasswordEncoder().encode(managerMemberEditDTO.getPassword()));
        }
        //查询会员信息
        Member member = this.getById(managerMemberEditDTO.getId());
        //传递修改会员信息
        BeanUtil.copyProperties(managerMemberEditDTO, member);
        this.updateById(member);
        return member;
    }

    @Override
    public IPage<MemberVO> getMemberPage(MemberSearchVO memberSearchVO, PageVO page) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        //用户名查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getUsername()), "username", memberSearchVO.getUsername());
        //用户名查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getNickName()), "nick_name", memberSearchVO.getNickName());
        //按照电话号码查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getMobile()), "mobile", memberSearchVO.getMobile());
        //按照会员状态查询
        queryWrapper.eq(CharSequenceUtil.isNotBlank(memberSearchVO.getDisabled()), "disabled",
                memberSearchVO.getDisabled().equals(SwitchEnum.OPEN.name()) ? 1 : 0);
        queryWrapper.orderByDesc("create_time");
        return this.baseMapper.pageByMemberVO(PageUtil.initPage(page), queryWrapper);
    }

    @Override
    @PointLogPoint
    @Transactional(rollbackFor = Exception.class)
    public Boolean updateMemberPoint(Long point, String type, String memberId, String content) {
        //获取当前会员信息
        Member member = this.getById(memberId);
        if (member != null) {
            //积分变动后的会员积分
            long currentPoint;
            //会员总获得积分
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
                //发送会员消息
                MemberPointMessage memberPointMessage = new MemberPointMessage();
                memberPointMessage.setPoint(point);
                memberPointMessage.setType(type);
                memberPointMessage.setMemberId(memberId);
                applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("update member point", rocketmqCustomProperties.getMemberTopic(), MemberTagsEnum.MEMBER_POINT_CHANGE.name(), memberPointMessage));
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

        return this.update(updateWrapper);
    }

    /**
     * 根据手机号获取会员
     *
     * @param mobilePhone 手机号
     * @return 会员
     */
    private Long findMember(String mobilePhone, String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("mobile", mobilePhone)
                .or().eq("username", userName);
        return this.baseMapper.selectCount(queryWrapper);
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
     * 成功登录，则检测cookie中的信息，进行会员绑定
     *
     * @param member  会员
     * @param unionId unionId
     * @param type    状态
     */
    private void loginBindUser(Member member, String unionId, String type) {
        Connect connect = connectService.queryConnect(
                ConnectQueryDTO.builder().unionId(unionId).unionType(type).build()
        );
        if (connect == null) {
            connect = new Connect(member.getId(), unionId, type);
            connectService.save(connect);
        }
    }

    /**
     * 成功登录，则检测cookie中的信息，进行会员绑定
     *
     * @param member 会员
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
     * 检测是否可以绑定第三方联合登陆
     * 返回null原因
     * 包含原因1：redis中已经没有联合登陆信息  2：已绑定其他账号
     *
     * @return 返回对象则代表可以进行绑定第三方会员，返回null则表示联合登陆无法继续
     */
    private ConnectAuthUser checkConnectUser() {
        //获取cookie存储的信息
        String uuid = CookieUtil.getCookie(ConnectService.CONNECT_COOKIE, ThreadContextHolder.getHttpRequest());
        String connectType = CookieUtil.getCookie(ConnectService.CONNECT_TYPE, ThreadContextHolder.getHttpRequest());

        //如果联合登陆存储了信息
        if (CharSequenceUtil.isNotEmpty(uuid) && CharSequenceUtil.isNotEmpty(connectType)) {
            //枚举 联合登陆类型获取
            ConnectAuthEnum authInterface = ConnectAuthEnum.valueOf(connectType);

            ConnectAuthUser connectAuthUser = getConnectAuthUser(uuid, connectType);
            if (connectAuthUser == null) {
                throw new ServiceException(ResultCode.USER_OVERDUE_CONNECT_ERROR);
            }
            //检测是否已经绑定过用户
            Connect connect = connectService.queryConnect(
                    ConnectQueryDTO.builder().unionType(connectType).unionId(connectAuthUser.getUuid()).build()
            );
            //没有关联则返回true，表示可以继续绑定
            if (connect == null) {
                connectAuthUser.setConnectEnum(authInterface);
                return connectAuthUser;
            } else {
                throw new ServiceException(ResultCode.USER_CONNECT_BANDING_ERROR);
            }
        } else {
            throw new ServiceException(ResultCode.USER_CONNECT_NOT_EXIST_ERROR);
        }
    }

    @Override
    public long getMemberNum(MemberSearchVO memberSearchVO) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        //用户名查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getUsername()), "username", memberSearchVO.getUsername());
        //按照电话号码查询
        queryWrapper.like(CharSequenceUtil.isNotBlank(memberSearchVO.getMobile()), "mobile", memberSearchVO.getMobile());
        //按照状态查询
        queryWrapper.eq(CharSequenceUtil.isNotBlank(memberSearchVO.getDisabled()), "disabled",
                memberSearchVO.getDisabled().equals(SwitchEnum.OPEN.name()) ? 1 : 0);
        queryWrapper.orderByDesc("create_time");
        return this.count(queryWrapper);
    }

    /**
     * 获取指定会员数据
     *
     * @param columns   指定获取的列
     * @param memberIds 会员ids
     * @return 指定会员数据
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
        if (CharSequenceUtil.isNotEmpty(currentUserToken)) {
            cache.remove(CachePrefix.ACCESS_TOKEN.getPrefix(userEnums) + currentUserToken);
        }
    }

    /**
     * 获取所有会员的手机号
     *
     * @return 所有会员的手机号
     */
    @Override
    public List<String> getAllMemberMobile() {
        return this.baseMapper.getAllMemberMobile();
    }

    /**
     * 更新会员登录时间为最新时间
     *
     * @param memberId 会员id
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
        return new MemberVO(this.getById(id));
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

    /**
     * 检测会员
     *
     * @param userName    会员名称
     * @param mobilePhone 手机号
     */
    private void checkMember(String userName, String mobilePhone) {
        //判断手机号是否存在
        if (findMember(mobilePhone, userName) > 0) {
            throw new ServiceException(ResultCode.USER_EXIST);
        }
    }
}