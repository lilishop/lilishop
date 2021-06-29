package cn.lili.modules.member.entity.dos;

import cn.lili.base.BaseEntity;
import cn.lili.common.validation.Phone;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;

/**
 * 会员地址
 *
 * @author Chopper
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_member_address")
@TableName("li_member_address")
@ApiModel(value = "会员地址")
public class MemberAddress extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "会员ID", hidden = true)
    private String memberId;

    @NotEmpty(message = "收货人姓名不能为空")
    @ApiModelProperty(value = "收货人姓名")
    private String name;

    @Phone
    @ApiModelProperty(value = "手机号码")
    private String mobile;

    @NotBlank(message = "地址不能为空")
    @ApiModelProperty(value = "地址名称， '，'分割")
    private String consigneeAddressPath;

    @NotBlank(message = "地址不能为空")
    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String consigneeAddressIdPath;

    @NotEmpty(message = "详细地址不能为空")
    @ApiModelProperty(value = "详细地址")
    private String detail;

    @ApiModelProperty(value = "是否为默认收货地址")
    private Boolean isDefault;

    @ApiModelProperty(value = "地址别名")
    private String alias;

    @ApiModelProperty(value = "经度")
    private String lon;

    @ApiModelProperty(value = "纬度")
    private String lat;
}