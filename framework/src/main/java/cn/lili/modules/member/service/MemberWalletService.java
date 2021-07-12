package cn.lili.modules.member.service;


import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberWallet;
import cn.lili.modules.member.entity.dos.MemberWithdrawApply;
import cn.lili.modules.member.entity.vo.MemberWalletVO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 会员预存款业务层
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
public interface MemberWalletService extends IService<MemberWallet> {

    /**
     * 查询会员的预存款
     *
     * @param memberId 会员id
     * @return 会员预存款VO
     */
    MemberWalletVO getMemberWallet(String memberId);

    /**
     * 增加用户预存款余额
     *
     * @param money       金额
     * @param memberId    会员id
     * @param serviceType 业务类型 @see DepositServiceTypeEnum
     * @param detail      操作描述
     * @return 返回增加结果    true:增加成功    false:增加失败
     */
    Boolean increase(Double money, String memberId, String detail, String serviceType);

    /**
     * 从冻结金额到余额
     *
     * @param money       金额
     * @param memberId    会员id
     * @param serviceType 业务类型 @see DepositServiceTypeEnum
     * @param detail      操作描述
     * @return 返回增加结果    true:增加成功    false:增加失败
     */
    Boolean increaseWithdrawal(Double money, String memberId, String detail, String serviceType);

    /**
     * 扣减用户预存款余额
     *
     * @param money       金额
     * @param memberId    会员id
     * @param detail      操作描述
     * @param serviceType 业务类型 @see DepositServiceTypeEnum
     * @return 操作状态
     */
    Boolean reduce(Double money, String memberId, String detail, String serviceType);

    /**
     * 提现扣减余额到冻结金额
     *
     * @param money       金额
     * @param memberId    会员id
     * @param detail      操作描述
     * @param serviceType 业务类型 @see DepositServiceTypeEnum
     * @return 操作状态
     */
    Boolean reduceWithdrawal(Double money, String memberId, String detail, String serviceType);

    /**
     * 提现扣减冻结金额
     *
     * @param money       金额
     * @param memberId    会员id
     * @param detail      操作描述
     * @param serviceType 类型
     * @return 操作状态
     */
    Boolean reduceFrozen(Double money, String memberId, String detail, String serviceType);

    /**
     * 设置支付密码
     *
     * @param member   会员id
     * @param password 支付密码
     */
    void setMemberWalletPassword(Member member, String password);

    /**
     * 检查当前会员是否设置过预存款密码
     *
     * @return 操作状态
     */
    Boolean checkPassword();

    /**
     * 会员注册添加会员预存款
     *
     * @param memberId   会员id
     * @param memberName 会员名称
     * @return 操作结果
     */
    MemberWallet save(String memberId, String memberName);

    /**
     * 用户提现
     *
     * @param price 提现金额
     * @return 是否提现成功
     */
    Boolean applyWithdrawal(Double price);

    /**
     * 提现公共方法，此方法供前端用户提现和后端提现使用
     *
     * @param memberWithdrawApply 会员零钱提现申请
     * @return 操作状态
     */
    Boolean withdrawal(MemberWithdrawApply memberWithdrawApply);

}