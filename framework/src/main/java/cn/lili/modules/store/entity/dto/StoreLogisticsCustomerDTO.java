package cn.lili.modules.store.entity.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 商家使用电子面单客户号DTO
 *
 * @author chc
 * @since 2022-4-13 10:00:58
 */
@Data
@ApiModel
public class StoreLogisticsCustomerDTO {

    @ApiModelProperty(value = "客户代码")
    private String customerName;

    @ApiModelProperty(value = "客户密码")
    private String customerPwd;

    @ApiModelProperty(value = "密钥")
    private String monthCode;

    @ApiModelProperty(value = "归属网点/网点编码")
    private String sendSite;

    @ApiModelProperty(value = "收件快递员")
    private String sendStaff;

    @ApiModelProperty(value = "是否使用电子面单")
    private boolean faceSheetFlag;

    @ApiModelProperty(value = "支付方式")
    private String payType;

    @ApiModelProperty(value = "快递类型")
    private String expType;

}
