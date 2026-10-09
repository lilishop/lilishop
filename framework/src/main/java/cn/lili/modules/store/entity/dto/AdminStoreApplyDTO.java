package cn.lili.modules.store.entity.dto;

import cn.lili.common.validation.Mobile;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import jakarta.validation.constraints.*;

/**
 * 后台添加店铺信息DTO
 *
 * @author Bulbasaur
 * @since 2020/12/12 11:35
 */
@Data
public class AdminStoreApplyDTO {

    /****店铺基本信息***/
    @Schema(description = "客户ID")
    public String memberId;

    @Size(min = 2, max = 200, message = "店铺名称长度为2-200位")
    @NotBlank(message = "店铺名称不能为空")
    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "店铺logo")
    private String storeLogo;

    @Size(min = 6, max = 200, message = "店铺简介需在6-200字符之间")
    @NotBlank(message = "店铺简介不能为空")
    @Schema(description = "店铺简介")
    private String storeDesc;

    @Schema(description = "经纬度")
    private String storeCenter;

    @Schema(description = "店铺经营类目")
    private String goodsManagementCategory;

    @Schema(description = "是否自营")
    private Boolean selfOperated;

    @Schema(description = "地址名称， '，'分割")
    private String storeAddressPath;

    @Schema(description = "地址id，'，'分割 ")
    private String storeAddressIdPath;

    @Schema(description = "详细地址")
    private String storeAddressDetail;

    /****公司基本信息***/
    @NotBlank(message = "公司名称不能为空")
    @Size(min = 2, max = 100, message = "公司名称错误")
    @Schema(description = "公司名称")
    private String companyName;

    @Mobile
    @Schema(description = "公司电话")
    private String companyPhone;

    @NotBlank(message = "公司地址不能为空")
    @Size(min = 1, max = 200, message = "公司地址,长度为1-200字符")
    @Schema(description = "公司地址")
    private String companyAddress;

    @Schema(description = "公司地址地区Id")
    private String companyAddressIdPath;

    @Schema(description = "公司地址地区")
    private String companyAddressPath;

    @Schema(description = "员工总数")
    private String employeeNum;

    @Min(value = 1, message = "注册资金,至少一位")
    @Schema(description = "注册资金")
    private Double registeredCapital;

    @NotBlank(message = "联系人姓名为空")
    @Length(min = 2, max = 20, message = "联系人长度为：2-20位字符")
    @Schema(description = "联系人姓名")
    private String linkName;

    @NotBlank(message = "手机号不能为空")
    @Pattern(regexp = "^[1][3,4,5,6,7,8,9][0-9]{9}$", message = "手机号格式有误")
    @Schema(description = "联系人电话")
    private String linkPhone;

    @Email
    @Schema(description = "电子邮箱")
    private String companyEmail;


    /****营业执照信息***/
    @NotBlank(message = "营业执照号不能为空")
    @Size(min = 18, max = 18, message = "营业执照长度为18位字符")
    @Schema(description = "营业执照号")
    private String licenseNum;

    @Schema(description = "法定经营范围")
    private String scope;

    @NotBlank(message = "营业执照电子版不能为空")
    @Schema(description = "营业执照电子版")
    private String licencePhoto;

    /****法人信息***/
    @NotBlank(message = "法人姓名不能为空")
    @Size(min = 2, max = 20, message = "法人姓名长度为2-20位字符")
    @Schema(description = "法人姓名")
    private String legalName;

    @NotBlank(message = "法人身份证不能为空")
    @Size(min = 18, max = 18, message = "法人身份证号长度为18位")
    @Schema(description = "法人身份证")
    private String legalId;

    @NotBlank(message = "法人身份证不能为空")
    @Schema(description = "法人身份证照片")
    private String legalPhoto;

    /****结算银行信息***/
    @Size(min = 1, max = 200, message = "结算银行开户行名称长度为1-200位")
    @NotBlank(message = "结算银行开户行名称不能为空")
    @Schema(description = "结算银行开户行名称")
    private String settlementBankAccountName;

    @Size(min = 1, max = 200, message = "结算银行开户账号长度为1-200位")
    @NotBlank(message = "结算银行开户账号不能为空")
    @Schema(description = "结算银行开户账号")
    private String settlementBankAccountNum;

    @Size(min = 1, max = 200, message = "结算银行开户支行名称长度为1-200位")
    @NotBlank(message = "结算银行开户支行名称不能为空")
    @Schema(description = "结算银行开户支行名称")
    private String settlementBankBranchName;

    @Size(min = 1, max = 50, message = "结算银行支行联行号长度为1-200位")
    @NotBlank(message = "结算银行支行联行号不能为空")
    @Schema(description = "结算银行支行联行号")
    private String settlementBankJointName;

    /****售后收货地址***/
    @Schema(description = "收货人姓名")
    private String salesConsigneeName;

    @Schema(description = "收件人手机")
    private String salesConsigneeMobile;

    @Schema(description = "地址Id， '，'分割")
    private String salesConsigneeAddressId;

    @Schema(description = "地址名称， '，'分割")
    private String salesConsigneeAddressPath;

    @Schema(description = "详细地址")
    private String salesConsigneeDetail;

    /****其他信息***/

    @Schema(description = "同城配送达达店铺编码")
    private String ddCode;

    /****结算日***/
    @Schema(description = "结算日")
    private String settlementCycle;

}
