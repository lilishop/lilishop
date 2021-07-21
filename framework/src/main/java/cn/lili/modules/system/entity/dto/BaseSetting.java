package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
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

    @ApiModelProperty(value = "站点名称")
    private String siteName;

    @ApiModelProperty(value = "icp")
    private String icp;

    @ApiModelProperty(value = "后端logo")
    private String domainLogo;

    @ApiModelProperty(value = "买家端logo")
    private String buyerSideLogo;

    @ApiModelProperty(value = "商家端logo")
    private String storeSideLogo;

    @ApiModelProperty(value = "站点地址")
    private String staticPageAddress;

    @ApiModelProperty(value = "wap站点地址")
    private String staticPageWapAddress;
}
