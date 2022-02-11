package cn.lili.modules.wallet.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 会员预存款
 *
 * @author pikachu
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_member_wallet")
@ApiModel(value = "会员预存款")
public class MemberWallet extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "会员用户名")
    private String memberName;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "会员预存款")
    private Double memberWallet;

    @ApiModelProperty(value = "会员预存款冻结金额,提现使用")
    private Double memberFrozenWallet;

    @ApiModelProperty(value = "预存款密码")
    private String walletPassword;

}
