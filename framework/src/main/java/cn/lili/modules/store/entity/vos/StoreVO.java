package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.dos.Store;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 店铺VO
 *
 * @author pikachu
 * @since 2020-03-07 17:02:05
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class StoreVO extends Store {

    /**
     * 登录用户的昵称
     */
    @Schema(description = "登录用户的昵称")
    private String nickName;

    /**
     * 店铺订单总数
     */
    @Schema(description = "订单总数")
    private Long orderNum;

    /**
     * 公司名称，关联 li_store_detail.company_name
     */
    @Schema(description = "公司名称")
    private String companyName;

    /**
     * 公司详细地址，关联 li_store_detail.company_address
     */
    @Schema(description = "公司地址")
    private String companyAddress;

    /**
     * 公司所在地地区ID路径（逗号分隔），关联 li_store_detail.company_address_id_path
     */
    @Schema(description = "公司地址地区Id")
    private String companyAddressIdPath;

    /**
     * 公司所在地地区名称路径，关联 li_store_detail.company_address_path
     */
    @Schema(description = "公司地址地区")
    private String companyAddressPath;

    /**
     * 公司电话，关联 li_store_detail.company_phone
     */
    @Schema(description = "公司电话")
    private String companyPhone;

    /**
     * 电子邮箱，关联 li_store_detail.company_email
     */
    @Schema(description = "电子邮箱")
    private String companyEmail;

    /**
     * 员工总数（单位：人），关联 li_store_detail.employee_num
     */
    @Schema(description = "员工总数")
    private Integer employeeNum;

    /**
     * 注册资金（单位：万元），关联 li_store_detail.registered_capital
     */
    @Schema(description = "注册资金")
    private Double registeredCapital;

    /**
     * 联系人姓名，关联 li_store_detail.link_name
     */
    @Schema(description = "联系人姓名")
    private String linkName;

    /**
     * 联系人电话，关联 li_store_detail.link_phone
     */
    @Schema(description = "联系人电话")
    private String linkPhone;

    /**
     * 营业执照号，关联 li_store_detail.license_num
     */
    @Schema(description = "营业执照号")
    private String licenseNum;

    /**
     * 法定经营范围，关联 li_store_detail.scope
     */
    @Schema(description = "法定经营范围")
    private String scope;

    /**
     * 营业执照电子版图片URL，关联 li_store_detail.licence_photo
     */
    @Schema(description = "营业执照电子版")
    private String licencePhoto;

    /**
     * 法人姓名，关联 li_store_detail.legal_name
     */
    @Schema(description = "法人姓名")
    private String legalName;

    /**
     * 法人身份证号，关联 li_store_detail.legal_id
     */
    @Schema(description = "法人身份证")
    private String legalId;

    /**
     * 法人身份证照片URL（正反面逗号分隔），关联 li_store_detail.legal_photo
     */
    @Schema(description = "法人身份证照片")
    private String legalPhoto;

    /**
     * 结算银行开户行名称，关联 li_store_detail.settlement_bank_account_name
     */
    @Schema(description = "结算银行开户行名称")
    private String settlementBankAccountName;

    /**
     * 结算银行开户账号，关联 li_store_detail.settlement_bank_account_num
     */
    @Schema(description = "结算银行开户账号")
    private String settlementBankAccountNum;

    /**
     * 结算银行开户支行名称，关联 li_store_detail.settlement_bank_branch_name
     */
    @Schema(description = "结算银行开户支行名称")
    private String settlementBankBranchName;

    /**
     * 结算银行支行联行号，关联 li_store_detail.settlement_bank_joint_name
     */
    @Schema(description = "结算银行支行联行号")
    private String settlementBankJointName;

    /**
     * 店铺签约经营的一级商品分类ID集合（逗号分隔），关联 li_store_detail.goods_management_category
     */
    @Schema(description = "店铺经营类目")
    private String goodsManagementCategory;

    /**
     * 结算周期（每月结算日，逗号分隔），关联 li_store_detail.settlement_cycle
     */
    @Schema(description = "结算日")
    private String settlementCycle;

}
