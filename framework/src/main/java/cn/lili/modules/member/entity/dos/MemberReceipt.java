package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 会员发票
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
@Data
@TableName("li_member_receipt")
@ApiModel(value = "会员发票")
public class MemberReceipt extends BaseIdEntity {

    private static final long serialVersionUID = -8210927482915675995L;

    @ApiModelProperty(value = "发票抬头")
    private String receiptTitle;

    @ApiModelProperty(value = "纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty(value = "发票内容")
    private String receiptContent;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    /**
     * @see cn.lili.modules.member.entity.enums.MemberReceiptEnum
     */
    @ApiModelProperty(value = "发票类型")
    private String receiptType;

    @ApiModelProperty(value = "是否为默认选项 0：否，1：是")
    private Integer isDefault;

    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "删除标志 true/false 删除/未删除", hidden = true)
    private Boolean deleteFlag;


    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;


}
