package cn.lili.modules.member.entity.vo;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.member.entity.dos.MemberReceipt;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 会员发票查询VO
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
@Data
@ApiModel(value = "会员发票")
public class MemberReceiptVO {

    private static final long serialVersionUID = -8210927982915677995L;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    /**
     * @see cn.lili.modules.member.entity.enums.MemberReceiptEnum
     */
    @ApiModelProperty(value = "发票类型")
    private String receiptType;

    public LambdaQueryWrapper<MemberReceipt> lambdaQueryWrapper() {
        LambdaQueryWrapper<MemberReceipt> queryWrapper = new LambdaQueryWrapper<>();

        //会员名称查询
        if (StringUtils.isNotEmpty(memberName)) {
            queryWrapper.like(MemberReceipt::getMemberName, memberName);
        }
        //会员id查询
        if (StringUtils.isNotEmpty(memberId)) {
            queryWrapper.eq(MemberReceipt::getMemberId, memberId);
        }
        //会员id查询
        if (StringUtils.isNotEmpty(receiptType)) {
            queryWrapper.eq(MemberReceipt::getReceiptType, receiptType);
        }
        queryWrapper.eq(MemberReceipt::getDeleteFlag, true);
        queryWrapper.orderByDesc(MemberReceipt::getCreateTime);
        return queryWrapper;
    }

}
