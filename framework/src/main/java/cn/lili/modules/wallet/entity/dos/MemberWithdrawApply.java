package cn.lili.modules.wallet.entity.dos;

import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.modules.wallet.entity.enums.WithdrawStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 会员提现申请
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_member_withdraw_apply")
@ApiModel(value = "会员提现申请")
public class MemberWithdrawApply extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "申请提现金额")
    private Double applyMoney;

    /**
     * @see WithdrawStatusEnum
     */
    @ApiModelProperty(value = "提现状态")
    private String applyStatus;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String memberName;

    @ApiModelProperty(value = "审核备注")
    private String inspectRemark;

    @ApiModelProperty(value = "审核时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    private Date inspectTime;

    @ApiModelProperty(value = "sn")
    private String sn;

}
