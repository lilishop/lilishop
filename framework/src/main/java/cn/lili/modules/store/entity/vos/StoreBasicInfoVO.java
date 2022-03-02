package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺基本信息DTO
 *
 * @author Bulbasaur
 * @since 2020/12/7 14:43
 */
@Data
public class StoreBasicInfoVO {

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    /**
     * @see StoreStatusEnum
     */
    @ApiModelProperty(value = "店铺状态")
    private String storeDisable;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String companyAddressPath;

    @ApiModelProperty(value = "店铺logo")
    private String storeLogo;

    @ApiModelProperty(value = "店铺简介")
    private String storeDesc;

    @ApiModelProperty(value = "PC端页面")
    private String pcPageData;

    @ApiModelProperty(value = "移动端页面")
    private String mobilePageData;

    @ApiModelProperty(value = "是否自营")
    private String selfOperated;

    @ApiModelProperty(value = "商品数量")
    private Integer goodsNum;

    @ApiModelProperty(value = "收藏数量")
    private Integer collectionNum;

    @ApiModelProperty(value = "腾讯云智服唯一标识")
    private String yzfSign;

    @ApiModelProperty(value = "腾讯云智服小程序唯一标识")
    private String yzfMpSign;

    @ApiModelProperty(value = "udesk标识")
    private String merchantEuid;

    @ApiModelProperty
    private String pageShow;

}
