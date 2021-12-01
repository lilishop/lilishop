package cn.lili.modules.wallet.service;


import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.wallet.entity.dos.MemberWallet;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.vo.MemberWalletVO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 会员预存款业务层
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
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
     * @param memberWalletUpdateDTO 变动模型
     * @return 返回增加结果    true:成功    false:失败
     */
    Boolean increase(MemberWalletUpdateDTO memberWalletUpdateDTO);

    /**
     * 从冻结金额到余额
     *
     * @param memberWalletUpdateDTO 变动模型
     * @return 返回冻结结果    true:成功    false:失败
     */
    Boolean increaseWithdrawal(MemberWalletUpdateDTO memberWalletUpdateDTO);

    /**
     * 扣减用户预存款余额
     *
     * @param memberWalletUpdateDTO 变动模型
     * @return 操作状态 true:成功    false:失败
     */
    Boolean reduce(MemberWalletUpdateDTO memberWalletUpdateDTO);

    /**
     * 提现扣减余额到冻结金额
     *
     * @param memberWalletUpdateDTO 变动模型
     * @return 操作状态 true:成功    false:失败
     */
    Boolean reduceWithdrawal(MemberWalletUpdateDTO memberWalletUpdateDTO);

    /**
     * 提现扣减冻结金额
     *
     * @param memberWalletUpdateDTO 变动模型
     * @return 操作状态
     */
    Boolean reduceFrozen(MemberWalletUpdateDTO memberWalletUpdateDTO);

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