package cn.lili.modules.member.serviceimpl;


import cn.hutool.core.convert.Convert;
import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.common.rocketmq.tags.MemberTagsEnum;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.token.Token;
import cn.lili.common.token.base.generate.MemberTokenGenerate;
import cn.lili.common.token.base.generate.StoreTokenGenerate;
import cn.lili.common.utils.*;
import cn.lili.common.vo.PageVO;
import cn.lili.config.context.ThreadContextHolder;
import cn.lili.config.rocketmq.RocketmqCustomProperties;
import cn.lili.modules.connect.config.ConnectAuthEnum;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.connect.util.UuidUtils;
import cn.lili.modules.member.entity.aop.annotation.PointLogPoint;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ManagerMemberEditDTO;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.entity.dto.MemberEditDTO;
import cn.lili.modules.member.entity.dto.MemberPointMessage;
import cn.lili.modules.member.entity.vo.MemberDistributionVO;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.utils.CharacterConstant;
import cn.lili.modules.system.utils.SensitiveWordsFilter;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * 会员接口业务层实现
 *
 * @author Chopper
 * @date 2021-03-29 14:10:16
 */
@Service
@Transactional(rollbackFor = Exception.class)
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
    /**
     * RocketMQ
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;

    @Override
    public Member findByUsername(String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper();
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
        QueryWrapper<Member> queryWrapper = new QueryWrapper();
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
        return memberTokenGenerate.createToken(member.getUsername(), false);
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
        if (member.getHaveStore()) {
            Store store = storeService.getById(member.getStoreId());
            if (!store.getStoreDisable().equals(StoreStatusEnum.OPEN.name())) {
                throw new ServiceException(ResultCode.STORE_CLOSE_ERROR);
            }
        } else {
            throw new ServiceException(ResultCode.USER_NOT_EXIST);
        }

        return storeTokenGenerate.createToken(member.getUsername(), false);
    }

    /**
     * 传递手机号或者用户名
     *
     * @param userName
     * @return
     */
    private Member findMember(String userName) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper();
        queryWrapper.eq("username", userName).or().eq("mobile", userName);
        return this.getOne(queryWrapper);
    }

    @Override
    public Token autoRegister(ConnectAuthUser authUser) {

        if (StringUtils.isEmpty(authUser.getNickname())) {
            authUser.setNickname("临时昵称");
        }
        if (StringUtils.isEmpty(authUser.getAvatar())) {
            authUser.setAvatar("https://i.loli.net/2020/11/19/LyN6JF7zZRskdIe.png");
        }
        try {
            String username = UuidUtils.getUUID();
            Member member = new Member(username, UuidUtils.getUUID(), authUser.getAvatar(), authUser.getNickname(),
                    authUser.getGender() != null ? Convert.toInt(authUser.getGender().getCode()) : 0);
            //保存会员
            this.save(member);
            Member loadMember = this.findByUsername(username);
            //绑定登录方式
            loginBindUser(loadMember, authUser.getUuid(), authUser.getSource());
            return memberTokenGenerate.createToken(username, false);
        } catch (ServiceException e) {
            log.error("自动注册服务泡出异常：", e);
            throw e;
        } catch (Exception e) {
            log.error("自动注册异常：", e);
            throw new ServiceException(ResultCode.USER_AUTO_REGISTER_ERROR);
        }
    }

    @Override
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
    public Token mobilePhoneLogin(String mobilePhone) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper();
        queryWrapper.eq("mobile", mobilePhone);
        Member member = this.baseMapper.selectOne(queryWrapper);
        //如果手机号不存在则自动注册用户
        if (member == null) {
            member = new Member(mobilePhone, UuidUtils.getUUID(), mobilePhone);
            //保存会员
            this.save(member);
            String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_REGISTER.name();
            rocketMQTemplate.asyncSend(destination, member, RocketmqSendCallbackBuilder.commonCallback());
        }
        loginBindUser(member);
        return memberTokenGenerate.createToken(member.getUsername(), false);
    }

    @Override
    public Member editOwn(MemberEditDTO memberEditDTO) {
        //查询会员信息
        Member member = this.findByUsername(UserContext.getCurrentUser().getUsername());
        //传递修改会员信息
        BeanUtil.copyProperties(memberEditDTO, member);
        //修改会员
        this.updateById(member);
        return member;
    }

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
        //修改会员密码
        LambdaUpdateWrapper<Member> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
        lambdaUpdateWrapper.eq(Member::getId, member.getId());
        lambdaUpdateWrapper.set(Member::getPassword, new BCryptPasswordEncoder().encode(newPassword));
        this.update(lambdaUpdateWrapper);
        return member;
    }

    @Override
    public Token register(String userName, String password, String mobilePhone) {
        //检测会员信息
        checkMember(userName, mobilePhone);
        //设置会员信息
        Member member = new Member(userName, new BCryptPasswordEncoder().encode(password), mobilePhone);
        //注册成功后用户自动登录
        if (this.save(member)) {
            Token token = memberTokenGenerate.createToken(member.getUsername(), false);
            String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_REGISTER.name();
            rocketMQTemplate.asyncSend(destination, member, RocketmqSendCallbackBuilder.commonCallback());
            return token;
        }
        return null;
    }

    @Override
    public boolean changeMobile(String mobile) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        Member member = this.findByUsername(tokenUser.getUsername());

        //判断是否用户登录并且会员ID为当前登录会员ID
        if (tokenUser == null || tokenUser.getId() != member.getId()) {
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
            return this.update(lambdaUpdateWrapper);
        } else {
            throw new ServiceException(ResultCode.USER_PHONE_NOT_EXIST);
        }

    }

    @Override
    public Member addMember(MemberAddDTO memberAddDTO) {

        //检测会员信息
        checkMember(memberAddDTO.getUsername(), memberAddDTO.getMobile());

        //添加会员
        Member member = new Member(memberAddDTO.getUsername(), memberAddDTO.getPassword(), memberAddDTO.getMobile());
        this.save(member);
        String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_REGISTER.name();
        rocketMQTemplate.asyncSend(destination, member, RocketmqSendCallbackBuilder.commonCallback());
        return member;
    }

    @Override
    public Member updateMember(ManagerMemberEditDTO managerMemberEditDTO) {
        //判断是否用户登录并且会员ID为当前登录会员ID
        AuthUser tokenUser = UserContext.getCurrentUser();
        if (tokenUser == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        //过滤会员昵称敏感词
        if (com.baomidou.mybatisplus.core.toolkit.StringUtils.isNotBlank(managerMemberEditDTO.getNickName())) {
            managerMemberEditDTO.setNickName(SensitiveWordsFilter.filter(managerMemberEditDTO.getNickName(), CharacterConstant.WILDCARD_STAR));
        }
        //如果密码不为空则加密密码
        if (com.baomidou.mybatisplus.core.toolkit.StringUtils.isNotBlank(managerMemberEditDTO.getPassword())) {
            managerMemberEditDTO.setPassword(new BCryptPasswordEncoder().encode(managerMemberEditDTO.getPassword()));
        }
        //查询会员信息
        Member member = this.findByUsername(managerMemberEditDTO.getUsername());
        //传递修改会员信息
        BeanUtil.copyProperties(managerMemberEditDTO, member);
        this.updateById(member);
        return member;
    }

    @Override
    public IPage<Member> getMemberPage(MemberSearchVO memberSearchVO, PageVO page) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        //用户名查询
        queryWrapper.like(StringUtils.isNotBlank(memberSearchVO.getUsername()), "username", memberSearchVO.getUsername());
        //用户名查询
        queryWrapper.like(StringUtils.isNotBlank(memberSearchVO.getNickName()), "nick_name", memberSearchVO.getNickName());
        //按照电话号码查询
        queryWrapper.like(StringUtils.isNotBlank(memberSearchVO.getMobile()), "mobile", memberSearchVO.getMobile());
        //按照会员状态查询
        queryWrapper.eq(StringUtils.isNotBlank(memberSearchVO.getDisabled()), "disabled",
                memberSearchVO.getDisabled().equals(SwitchEnum.OPEN.name()) ? 1 : 0);
        queryWrapper.orderByDesc("create_time");
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

    @Override
    @PointLogPoint
    public Boolean updateMemberPoint(Long point, Boolean type, String memberId, String content) {
        //获取当前会员信息
        Member member = this.getById(memberId);
        if (member != null) {
            //积分变动后的会员积分
            long currentPoint;
            if (type) {
                currentPoint = CurrencyUtil.add(member.getPoint(), point).longValue();
            } else {
                currentPoint = CurrencyUtil.sub(member.getPoint(), point) < 0 ? 0 : new Double(CurrencyUtil.sub(member.getPoint(), point)).longValue();
            }
            member.setPoint(currentPoint);
            Boolean result = this.updateById(member);
            if (result) {
                //发送会员消息
                MemberPointMessage memberPointMessage = new MemberPointMessage();
                memberPointMessage.setPoint(point);
                memberPointMessage.setType(type);
                memberPointMessage.setMemberId(memberId);
                String destination = rocketmqCustomProperties.getMemberTopic() + ":" + MemberTagsEnum.MEMBER_POINT_CHANGE.name();
                rocketMQTemplate.asyncSend(destination, memberPointMessage, RocketmqSendCallbackBuilder.commonCallback());
                return true;
            }
            return false;

        }
        throw new ServiceException(ResultCode.USER_NOT_EXIST);
    }

    @Override
    public Boolean updateMemberExperience(Long experience, Boolean type, String memberId, String content) {
        //获取当前会员信息
        Member member = this.getById(memberId);
        if (member != null) {
            //积分变动后的会员积分
            long currentExperience;
            if (type) {
                currentExperience = CurrencyUtil.add(member.getPoint(), experience).longValue();
            } else {
                currentExperience = CurrencyUtil.sub(member.getPoint(), experience) < 0 ? 0 : new Double(CurrencyUtil.sub(member.getExperience(), experience)).longValue();
            }
            member.setExperience(currentExperience);

            return this.updateById(member);
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

    @Override
    public List<MemberDistributionVO> distribution() {
        List<MemberDistributionVO> memberDistributionVOS = this.baseMapper.distribution();
        return memberDistributionVOS;
    }

    /**
     * 根据手机号获取会员
     *
     * @param mobilePhone 手机号
     * @return 会员
     */
    private Member findByPhone(String mobilePhone) {
        QueryWrapper<Member> queryWrapper = new QueryWrapper();
        queryWrapper.eq("mobile", mobilePhone);
        return this.baseMapper.selectOne(queryWrapper);
    }

    /**
     * 获取cookie中的联合登录对象
     *
     * @param uuid uuid
     * @param type 状态
     * @return
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
        LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Connect::getUnionId, unionId);
        queryWrapper.eq(Connect::getUnionType, type);
        Connect connect = connectService.getOne(queryWrapper);
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
        if (StringUtils.isNotEmpty(uuid) && StringUtils.isNotEmpty(connectType)) {
            try {
                //获取信息
                ConnectAuthUser connectAuthUser = getConnectAuthUser(uuid, connectType);
                if (connectAuthUser == null) {
                    return;
                }
                //检测是否已经绑定过用户
                LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();
                queryWrapper.eq(Connect::getUnionId, connectAuthUser.getUuid());
                queryWrapper.eq(Connect::getUnionType, connectType);
                Connect connect = connectService.getOne(queryWrapper);
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
        if (StringUtils.isNotEmpty(uuid) && StringUtils.isNotEmpty(connectType)) {
            try {
                //枚举 联合登陆类型获取
                ConnectAuthEnum authInterface = ConnectAuthEnum.valueOf(connectType);

                ConnectAuthUser connectAuthUser = getConnectAuthUser(uuid, connectType);
                if (connectAuthUser == null) {
                    throw new ServiceException(ResultCode.USER_OVERDUE_CONNECT_ERROR);
                }
                //检测是否已经绑定过用户
                LambdaQueryWrapper<Connect> queryWrapper = new LambdaQueryWrapper<>();
                queryWrapper.eq(Connect::getUnionId, connectAuthUser.getUuid());
                queryWrapper.eq(Connect::getUnionType, connectType);
                Connect connect = connectService.getOne(queryWrapper);
                //没有关联则返回true，表示可以继续绑定
                if (connect == null) {
                    connectAuthUser.setConnectEnum(authInterface);
                    return connectAuthUser;
                } else {
                    throw new ServiceException(ResultCode.USER_CONNECT_BANDING_ERROR);
                }
            } catch (Exception e) {
                throw e;
            }
        } else {
            throw new ServiceException(ResultCode.USER_CONNECT_NOT_EXIST_ERROR);
        }
    }

    @Override
    public Integer getMemberNum(MemberSearchVO memberSearchVO) {
        QueryWrapper<Member> queryWrapper = Wrappers.query();
        //用户名查询
        queryWrapper.like(StringUtils.isNotBlank(memberSearchVO.getUsername()), "username", memberSearchVO.getUsername());
        //按照电话号码查询
        queryWrapper.like(StringUtils.isNotBlank(memberSearchVO.getMobile()), "mobile", memberSearchVO.getMobile());
        //按照状态查询
        queryWrapper.eq(StringUtils.isNotBlank(memberSearchVO.getDisabled()), "disabled",
                memberSearchVO.getDisabled().equals(SwitchEnum.OPEN.name()) ? 1 : 0);        queryWrapper.orderByDesc("create_time");
        return this.count(queryWrapper);
    }

    /**
     * 检测会员
     *
     * @param userName    会员名称
     * @param mobilePhone 手机号
     */
    private void checkMember(String userName, String mobilePhone) {
        //判断用户名是否存在
        if (findByUsername(userName) != null) {
            throw new ServiceException(ResultCode.USER_NAME_EXIST);
        }
        //判断手机号是否存在
        if (findByPhone(mobilePhone) != null) {
            throw new ServiceException(ResultCode.USER_PHONE_EXIST);
        }
    }
}