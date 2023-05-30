package cn.lili.modules.wallet.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.vo.MemberWithdrawApplyQueryVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 会员提现申请业务层
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
public interface MemberWithdrawApplyService extends IService<MemberWithdrawApply> {

    /**
     * 平台审核提现申请，申请成功后直接扣款
     *
     * @param applyId 审核id
     * @param result  审核结构
     * @param remark  备注
     * @return 操作状态
     */
    Boolean audit(String applyId, Boolean result, String remark);

    /**
     * 提现记录列表
     *
     * @param pageVO                     分页条件
     * @param memberWithdrawApplyQueryVO 提现记录查询条件
     * @return 提现记录分页
     */
    IPage<MemberWithdrawApply> getMemberWithdrawPage(PageVO pageVO, MemberWithdrawApplyQueryVO memberWithdrawApplyQueryVO);

}