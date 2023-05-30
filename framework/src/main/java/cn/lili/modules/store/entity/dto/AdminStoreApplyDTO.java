package cn.lili.modules.store.entity.dto;

import cn.lili.common.validation.Mobile;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.*;

/**
 * 后台添加店铺信息DTO
 *
 * @author Bulbasaur
 * @since 2020/12/12 11:35
 */
@Data
public class AdminStoreApplyDTO {

    /****店铺基本信息***/
    @ApiModelProperty(value = "会员ID")
    public String memberId;

    @Size(min = 2, max = 200, message = "店铺名称长度为2-200位")
    @NotBlank(message = "店铺名称不能为空")
    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "店铺logo")
    private String storeLogo;

    @Size(min = 6, max = 200, message = "店铺简介需在6-200字符之间")
    @NotBlank(message = "店铺简介不能为空")
    @ApiModelProperty(value = "店铺简介")
    private String storeDesc;

    @ApiModelProperty(value = "经纬度")
    private String storeCenter;

    @ApiModelProperty(value = "店铺经营类目")
    private String goodsManagementCategory;

    @ApiModelProperty(value = "是否自营")
    private Boolean selfOperated;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String storeAddressPath;

    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String storeAddressIdPath;

    @ApiModelProperty(value = "详细地址")
    private String storeAddressDetail;

    /****公司基本信息***/
    @NotBlank(message = "公司名称不能为空")
    @Size(min = 2, max = 100, message = "公司名称错误")
    @ApiModelProperty(value = "公司名称")
    private String companyName;

    @Mobile
    @ApiModelProperty(value = "公司电话")
    private String companyPhone;

    @NotBlank(message = "公司地址不能为空")
    @Size(min = 1, max = 200, message = "公司地址,长度为1-200字符")
    @ApiModelProperty(value = "公司地址")
    private String companyAddress;

    @ApiModelProperty(value = "公司地址地区Id")
    private String companyAddressIdPath;

    @ApiModelProperty(value = "公司地址地区")
    private String companyAddressPath;

    @ApiModelProperty(value = "员工总数")
    private String employeeNum;

    @Min(value = 1, message = "注册资金,至少一位")
    @ApiModelProperty(value = "注册资金")
    private Double registeredCapital;

    @NotBlank(message = "联系人姓名为空")
    @Length(min = 2, max = 20, message = "联系人长度为：2-20位字符")
    @ApiModelProperty(value = "联系人姓名")
    private String linkName;

    @NotBlank(message = "手机号不能为空")
    @Pattern(regexp = "^[1][3,4,5,6,7,8,9][0-9]{9}$", message = "手机号格式有误")
    @ApiModelProperty(value = "联系人电话")
    private String linkPhone;

    @Email
    @ApiModelProperty(value = "电子邮箱")
    private String companyEmail;


    /****营业执照信息***/
    @Size(min = 18, max = 18, message = "营业执照长度为18位字符")
    @ApiModelProperty(value = "营业执照号")
    private String licenseNum;

    @ApiModelProperty(value = "法定经营范围")
    private String scope;

    @NotBlank(message = "营业执照电子版不能为空")
    @ApiModelProperty(value = "营业执照电子版")
    private String licencePhoto;

    /****法人信息***/
    @NotBlank(message = "法人姓名不能为空")
    @Size(min = 2, max = 20, message = "法人姓名长度为2-20位字符")
    @ApiModelProperty(value = "法人姓名")
    private String legalName;

    @NotBlank(message = "法人身份证不能为空")
    @Size(min = 18, max = 18, message = "法人身份证号长度为18位")
    @ApiModelProperty(value = "法人身份证")
    private String legalId;

    @NotBlank(message = "法人身份证不能为空")
    @ApiModelProperty(value = "法人身份证照片")
    private String legalPhoto;

    /****结算银行信息***/
    @Size(min = 1, max = 200, message = "结算银行开户行名称长度为1-200位")
    @NotBlank(message = "结算银行开户行名称不能为空")
    @ApiModelProperty(value = "结算银行开户行名称")
    private String settlementBankAccountName;

    @Size(min = 1, max = 200, message = "结算银行开户账号长度为1-200位")
    @NotBlank(message = "结算银行开户账号不能为空")
    @ApiModelProperty(value = "结算银行开户账号")
    private String settlementBankAccountNum;

    @Size(min = 1, max = 200, message = "结算银行开户支行名称长度为1-200位")
    @NotBlank(message = "结算银行开户支行名称不能为空")
    @ApiModelProperty(value = "结算银行开户支行名称")
    private String settlementBankBranchName;

    @Size(min = 1, max = 50, message = "结算银行支行联行号长度为1-200位")
    @NotBlank(message = "结算银行支行联行号不能为空")
    @ApiModelProperty(value = "结算银行支行联行号")
    private String settlementBankJointName;

    /****店铺退货收件地址***/
    @ApiModelProperty(value = "收货人姓名")
    private String salesConsigneeName;

    @ApiModelProperty(value = "收件人手机")
    private String salesConsigneeMobile;

    @ApiModelProperty(value = "地址Id， '，'分割")
    private String salesConsigneeAddressId;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String salesConsigneeAddressPath;

    @ApiModelProperty(value = "详细地址")
    private String salesConsigneeDetail;


    /****配送信息***/
    @ApiModelProperty(value = "同城配送达达店铺编码")
    private String ddCode;

    /****结算周期***/
    @ApiModelProperty(value = "结算周期")
    private String settlementCycle;

}
