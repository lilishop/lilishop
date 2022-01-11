package cn.lili.modules.store.entity.dos;


import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DateUtil;
import cn.lili.common.validation.Mobile;
import cn.lili.common.validation.Phone;
import cn.lili.modules.store.entity.dto.AdminStoreApplyDTO;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.hibernate.validator.constraints.Length;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.Email;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.util.Date;

/**
 * 店铺详细
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@TableName("li_store_detail")
@ApiModel(value = "店铺详细")
@NoArgsConstructor
public class StoreDetail extends BaseIdEntity {

    private static final long serialVersionUID = 4949782642253898816L;

    @NotBlank(message = "店铺不能为空")
    @ApiModelProperty(value = "店铺id")
    private String storeId;

    @Size(min = 2, max = 200, message = "店铺名称长度为2-200位")
    @NotBlank(message = "店铺名称不能为空")
    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @NotBlank(message = "公司名称不能为空")
    @Size(min = 2, max = 100, message = "公司名称错误")
    @ApiModelProperty(value = "公司名称")
    private String companyName;

    @NotBlank(message = "公司地址不能为空")
    @Size(min = 1, max = 200, message = "公司地址,长度为1-200字符")
    @ApiModelProperty(value = "公司地址")
    private String companyAddress;

    @ApiModelProperty(value = "公司地址地区Id")
    private String companyAddressIdPath;

    @ApiModelProperty(value = "公司地址地区")
    private String companyAddressPath;

    @Mobile
    @ApiModelProperty(value = "公司电话")
    private String companyPhone;

    @Email
    @ApiModelProperty(value = "电子邮箱")
    private String companyEmail;

    @Min(value = 1, message = "员工总数,至少一位")
    @ApiModelProperty(value = "员工总数")
    private Integer employeeNum;

    @Min(value = 1, message = "注册资金,至少一位")
    @ApiModelProperty(value = "注册资金")
    private Double registeredCapital;

    @NotBlank(message = "联系人姓名为空")
    @Length(min = 2, max = 20, message = "联系人长度为：2-20位字符")
    @ApiModelProperty(value = "联系人姓名")
    private String linkName;

    @NotBlank(message = "手机号不能为空")
    @Phone
    @ApiModelProperty(value = "联系人电话")
    private String linkPhone;

    @Size(min = 18, max = 18, message = "营业执照长度为18位字符")
    @ApiModelProperty(value = "营业执照号")
    private String licenseNum;

    @ApiModelProperty(value = "法定经营范围")
    private String scope;

    @NotBlank(message = "营业执照电子版不能为空")
    @ApiModelProperty(value = "营业执照电子版")
    private String licencePhoto;

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

    @NotBlank(message = "店铺经营类目不能为空")
    @ApiModelProperty(value = "店铺经营类目")
    private String goodsManagementCategory;

    @ApiModelProperty(value = "结算周期")
    private String settlementCycle;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "结算日", hidden = true)
    private Date settlementDay;


    @ApiModelProperty(value = "库存预警数量")
    private Integer stockWarning;

    /**
     * 同城配送达达店铺编码
     */
    @ApiModelProperty(value = "同城配送达达店铺编码")
    @TableField(value = "dd_code")
    private String ddCode;

    //店铺退货收件地址

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


    public StoreDetail(Store store, AdminStoreApplyDTO adminStoreApplyDTO) {
        this.storeId = store.getId();
        //设置店铺公司信息、设置店铺银行信息、设置店铺其他信息
        BeanUtil.copyProperties(adminStoreApplyDTO, this);
        this.settlementDay = DateUtil.date();
        this.stockWarning = 10;
    }
}