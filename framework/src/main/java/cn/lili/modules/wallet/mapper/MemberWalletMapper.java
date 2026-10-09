package cn.lili.modules.wallet.mapper;


import cn.lili.modules.wallet.entity.dos.MemberWallet;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Update;

/**
 * 客户预存款数据处理层
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
public interface MemberWalletMapper extends BaseMapper<MemberWallet> {

    /**
     * 余额原子增加
     */
    @Update("UPDATE li_member_wallet SET member_wallet = member_wallet + #{money} WHERE member_id = #{memberId}")
    int increaseWallet(@Param("memberId") String memberId, @Param("money") Double money);

    /**
     * 余额原子扣减，余额不足时影响行数为 0
     */
    @Update("UPDATE li_member_wallet SET member_wallet = member_wallet - #{money} WHERE member_id = #{memberId} AND member_wallet >= #{money}")
    int reduceWallet(@Param("memberId") String memberId, @Param("money") Double money);

    /**
     * 余额原子转入冻结（提现申请），余额不足时影响行数为 0
     */
    @Update("UPDATE li_member_wallet SET member_wallet = member_wallet - #{money}, member_frozen_wallet = member_frozen_wallet + #{money} WHERE member_id = #{memberId} AND member_wallet >= #{money}")
    int moveWalletToFrozen(@Param("memberId") String memberId, @Param("money") Double money);

    /**
     * 冻结原子退回余额（提现失败/审核拒绝），冻结不足时影响行数为 0
     */
    @Update("UPDATE li_member_wallet SET member_wallet = member_wallet + #{money}, member_frozen_wallet = member_frozen_wallet - #{money} WHERE member_id = #{memberId} AND member_frozen_wallet >= #{money}")
    int releaseFrozenToWallet(@Param("memberId") String memberId, @Param("money") Double money);

    /**
     * 冻结原子扣减（提现成功），冻结不足时影响行数为 0
     */
    @Update("UPDATE li_member_wallet SET member_frozen_wallet = member_frozen_wallet - #{money} WHERE member_id = #{memberId} AND member_frozen_wallet >= #{money}")
    int reduceFrozenWallet(@Param("memberId") String memberId, @Param("money") Double money);

}
