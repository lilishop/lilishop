package cn.lili.modules.order.trade.entity.dos;

import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 预存款日志实体
 *
 * @author pikachu
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_wallet_log")
@TableName("li_wallet_log")
@ApiModel(value = "钱包变动日志")
@NoArgsConstructor
public class WalletLog {

    private static final long serialVersionUID = -1599270544927161096L;

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    /**
     * 会员id
     */
    @ApiModelProperty(value = "会员id")
    private String memberId;

    /**
     * 会员名称
     */
    @ApiModelProperty(value = "会员名称")
    private String memberName;
    /**
     * 金额
     */
    @ApiModelProperty(value = "金额")
    private Double money;

    /**
     * 变动业务类型
     *
     * @see cn.lili.modules.order.trade.entity.enums.DepositServiceTypeEnum
     */
    @ApiModelProperty(value = "业务类型")
    private String serviceType;

    /**
     * 日志明细
     */
    @ApiModelProperty(value = "日志明细")
    private String detail;


    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    /**
     * 构建新的预存款日志对象
     *
     * @param memberId   会员id
     * @param memberName 会员名称
     * @param money      金额
     * @param detail     备注
     */
    public WalletLog(String memberId, String memberName, Double money, String detail, String serviceType) {
        this.setMemberId(memberId);
        this.setMemberName(memberName);
        this.setMoney(money);
        this.setDetail(detail);
        this.setServiceType(serviceType);
    }

}