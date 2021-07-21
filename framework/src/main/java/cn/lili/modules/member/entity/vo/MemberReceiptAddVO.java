package cn.lili.modules.member.entity.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 会员发票添加VO
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
@Data
@ApiModel(value = "会员发票")
public class MemberReceiptAddVO {

    private static final long serialVersionUID = -8267092982915677995L;

    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "发票抬头")
    private String receiptTitle;

    @ApiModelProperty(value = "纳税人识别号")
    private String taxpayerId;

    @ApiModelProperty(value = "发票内容")
    private String receiptContent;

    /**
     * @see cn.lili.modules.member.entity.enums.MemberReceiptEnum
     */
    @ApiModelProperty(value = "发票类型")
    private String receiptType;

    @ApiModelProperty(value = "是否为默认选项 0：否，1：是")
    private Integer isDefault;

}
