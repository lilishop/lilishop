package cn.lili.modules.store.entity.dto;

import cn.lili.common.validation.Mobile;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;

import javax.validation.constraints.*;

/**
 * 店铺-公司信息
 *
 * @author Bulbasaur
 * @since 2020/12/7 15:50
 */
@Data
public class StoreCompanyDTO {

    //公司基本信息

    @Size(min = 2, max = 100)
    @NotBlank(message = "公司名称不能为空")
    @ApiModelProperty(value = "公司名称")
    private String companyName;

    @ApiModelProperty(value = "公司地址地区Id")
    private String companyAddressIdPath;

    @ApiModelProperty(value = "公司地址地区")
    private String companyAddressPath;

    @Size(min = 1, max = 200)
    @NotBlank(message = "公司地址不能为空")
    @ApiModelProperty(value = "公司地址")
    private String companyAddress;

    @Mobile
    @ApiModelProperty(value = "公司电话")
    private String companyPhone;

    @Email
    @ApiModelProperty(value = "电子邮箱")
    private String companyEmail;

    @Min(1)
    @ApiModelProperty(value = "员工总数")
    private Integer employeeNum;

    @Min(1)
    @ApiModelProperty(value = "注册资金")
    private Double registeredCapital;

    @Length(min = 2, max = 20)
    @NotBlank(message = "联系人姓名为空")
    @ApiModelProperty(value = "联系人姓名")
    private String linkName;

    @NotBlank(message = "手机号不能为空")
    @Pattern(regexp = "^[1][3,4,5,6,7,8,9][0-9]{9}$", message = "手机号格式有误")
    @ApiModelProperty(value = "联系人电话")
    private String linkPhone;

    //营业执照信息

    @Size(min = 18, max = 18)
    @ApiModelProperty(value = "营业执照号")
    private String licenseNum;

    @Size(min = 1, max = 200)
    @ApiModelProperty(value = "法定经营范围")
    private String scope;

    @NotBlank(message = "营业执照电子版不能为空")
    @ApiModelProperty(value = "营业执照电子版")
    private String licencePhoto;

    //法人信息

    @Size(min = 2, max = 20)
    @NotBlank(message = "法人姓名不能为空")
    @ApiModelProperty(value = "法人姓名")
    private String legalName;

    @Size(min = 18, max = 18)
    @NotBlank(message = "法人身份证不能为空")
    @ApiModelProperty(value = "法人身份证")
    private String legalId;

    @NotBlank(message = "法人身份证不能为空")
    @ApiModelProperty(value = "法人身份证照片")
    private String legalPhoto;

}
