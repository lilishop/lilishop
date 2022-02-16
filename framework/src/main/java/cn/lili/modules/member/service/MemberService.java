package cn.lili.modules.member.service;


import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.token.Token;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.connect.entity.dto.ConnectAuthUser;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ManagerMemberEditDTO;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.entity.dto.MemberEditDTO;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 会员业务层
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
public interface MemberService extends IService<Member> {

    /**
     * 获取当前登录的用户信息
     *
     * @return 会员信息
     */
    Member getUserInfo();

    /**
     * 是否可以通过手机获取用户
     *
     * @param uuid   UUID
     * @param mobile 手机号
     * @return 操作状态
     */
    boolean findByMobile(String uuid, String mobile);

    /**
     * 通过用户名获取用户
     *
     * @param username 用户名
     * @return 会员信息
     */
    Member findByUsername(String username);

    /**
     * 登录：用户名、密码登录
     *
     * @param username 用户名
     * @param password 密码
     * @return token
     */
    Token usernameLogin(String username, String password);

    /**
     * 商家登录：用户名、密码登录
     *
     * @param username 用户名
     * @param password 密码
     * @return token
     */
    Token usernameStoreLogin(String username, String password);

    /**
     * 注册：手机号、验证码登录
     *
     * @param mobilePhone 手机号
     * @return token
     */
    Token mobilePhoneLogin(String mobilePhone);

    /**
     * 修改会员信息
     *
     * @param memberEditDTO 会员修改信息
     * @return 修改后的会员
     */
    Member editOwn(MemberEditDTO memberEditDTO);

    /**
     * 修改用户密码
     *
     * @param oldPassword 旧密码
     * @param newPassword 新密码
     * @return 操作结果
     */
    Member modifyPass(String oldPassword, String newPassword);

    /**
     * 注册会员
     *
     * @param userName    会员
     * @param password    密码
     * @param mobilePhone mobilePhone
     * @return 处理结果
     */
    Token register(String userName, String password, String mobilePhone);

    /**
     * 修改当前会员的手机号
     *
     * @param mobile 手机号
     * @return 操作结果
     */
    boolean changeMobile(String mobile);


    /**
     * 通过手机号修改密码
     *
     * @param mobile   手机号
     * @param password 密码
     * @return
     */
    boolean resetByMobile(String mobile, String password);

    /**
     * 后台-添加会员
     *
     * @param memberAddDTO 会员
     * @return 会员
     */
    Member addMember(MemberAddDTO memberAddDTO);

    /**
     * 后台-修改会员
     *
     * @param managerMemberEditDTO 后台修改会员参数
     * @return 会员
     */
    Member updateMember(ManagerMemberEditDTO managerMemberEditDTO);

    /**
     * 获取会员分页
     *
     * @param memberSearchVO 会员搜索VO
     * @param page           分页
     * @return 会员分页
     */
    IPage<MemberVO> getMemberPage(MemberSearchVO memberSearchVO, PageVO page);


    /**
     * 一键注册会员
     *
     * @return
     */
    Token autoRegister();

    /**
     * 一键注册会员
     *
     * @param authUser 联合登录用户
     * @return Token
     */
    Token autoRegister(ConnectAuthUser authUser);

    /**
     * 刷新token
     *
     * @param refreshToken
     * @return Token
     */
    Token refreshToken(String refreshToken);

    /**
     * 刷新token
     *
     * @param refreshToken
     * @return Token
     */
    Token refreshStoreToken(String refreshToken);

    /**
     * 会员积分变动
     *
     * @param point    变动积分
     * @param type     是否增加积分 INCREASE 增加  REDUCE 扣减
     * @param memberId 会员id
     * @param content  变动日志
     * @return 操作结果
     */
    Boolean updateMemberPoint(Long point, String type, String memberId, String content);


    /**
     * 修改会员状态
     *
     * @param memberIds 会员id集合
     * @param status    状态
     * @return 修改结果
     */
    Boolean updateMemberStatus(List<String> memberIds, Boolean status);

    /**
     * 根据条件查询会员总数
     *
     * @param memberSearchVO
     * @return 会员总数
     */
    long getMemberNum(MemberSearchVO memberSearchVO);

    /**
     * 获取指定会员数据
     *
     * @param columns 指定获取的列
     * @param memberIds 会员ids
     * @return 指定会员数据
     */
    List<Map<String, Object>> listFieldsByMemberIds(String columns, List<String> memberIds);

    /**
     * 登出
     *
     * @param userEnums token角色类型
     */
    void logout(UserEnums userEnums);

    /**
     * 获取所有会员的手机号
     *
     * @return 所有会员的手机号
     */
    List<String> getAllMemberMobile();

    /**
     * 更新会员登录时间为最新时间
     *
     * @param memberId 会员id
     * @return 是否更新成功
     */
    boolean updateMemberLoginTime(String memberId);

    /**
     * 获取用户VO
     * @param id 会员id
     * @return 用户VO
     */
    MemberVO getMember(String id);
}