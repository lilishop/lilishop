package cn.lili.modules.member.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.Date;

/**
 * 会员提现申请
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_member_withdraw_apply")
@TableName("li_member_withdraw_apply")
@ApiModel(value = "会员提现申请")
public class MemberWithdrawApply extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "申请提现金额")
    private Double applyMoney;

    @ApiModelProperty(value = "提现状态")
    private String applyStatus;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "审核备注")
    private String inspectRemark;

    @ApiModelProperty(value="审核时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    private Date inspectTime;

    @ApiModelProperty(value="sn")
    private String sn;

}
