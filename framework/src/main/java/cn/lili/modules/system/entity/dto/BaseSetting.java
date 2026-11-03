package cn.lili.modules.system.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 基础设置
 *
 * @author Chopper
 * @since 2020/11/17 7:58 下午
 */
@Data
public class BaseSetting implements Serializable {

    private static final long serialVersionUID = -3138023944444671722L;

    @Schema(description = "站点名称")
    private String siteName;

    @Schema(description = "icp")
    private String icp;

    @Schema(description = "后端logo")
    private String domainLogo;

    @Schema(description = "后端icon")
    private String domainIcon;

    @Schema(description = "买家端logo")
    private String buyerSideLogo;

    @Schema(description = "买家端icon")
    private String buyerSideIcon;

    @Schema(description = "商家端logo")
    private String storeSideLogo;

    @Schema(description = "商家端icon")
    private String storeSideIcon;

    @Schema(description = "站点地址")
    private String staticPageAddress;

    @Schema(description = "wap站点地址")
    private String staticPageWapAddress;

    @Schema(description = "公司名称")
    private String companyName;

    @Schema(description = "公司链接")
    private String companyUrl;

    @Schema(description = "版权归属")
    private String copyrightOwner;

    @Schema(description = "官网地址")
    private String website;
}
