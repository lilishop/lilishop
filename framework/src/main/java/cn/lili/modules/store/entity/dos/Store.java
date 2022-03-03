package cn.lili.modules.store.entity.dos;

import cn.lili.mybatis.BaseEntity;
import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.store.entity.dto.AdminStoreApplyDTO;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.Size;
import java.util.Date;

/**
 * 店铺
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
@Data
@TableName("li_store")
@ApiModel(value = "店铺")
@NoArgsConstructor
public class Store extends BaseEntity {

    private static final long serialVersionUID = -5861767726387892272L;

    @ApiModelProperty(value = "会员Id")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @JsonFormat(pattern = "yyyy-MM-dd", timezone = "GMT+8")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "店铺关闭时间")
    private Date storeEndTime;

    /**
     * @see StoreStatusEnum
     */
    @ApiModelProperty(value = "店铺状态")
    private String storeDisable;

    @ApiModelProperty(value = "是否自营")
    private Boolean selfOperated;

    @ApiModelProperty(value = "店铺logo")
    private String storeLogo;

    @ApiModelProperty(value = "经纬度")
    @NotEmpty
    private String storeCenter;

    @Size(min = 6, max = 200, message = "店铺简介需在6-200字符之间")
    @NotBlank(message = "店铺简介不能为空")
    @ApiModelProperty(value = "店铺简介")
    private String storeDesc;

    @ApiModelProperty(value = "地址名称， '，'分割")
    private String storeAddressPath;

    @ApiModelProperty(value = "地址id，'，'分割 ")
    private String storeAddressIdPath;

    @ApiModelProperty(value = "详细地址")
    private String storeAddressDetail;

    @ApiModelProperty(value = "描述评分")
    private Double descriptionScore;

    @ApiModelProperty(value = "服务评分")
    private Double serviceScore;

    @ApiModelProperty(value = "物流描述")
    private Double deliveryScore;

    @ApiModelProperty(value = "商品数量")
    private Integer goodsNum;

    @ApiModelProperty(value = "收藏数量")
    private Integer collectionNum;

    @ApiModelProperty(value = "腾讯云智服唯一标识")
    private String yzfSign;

    @ApiModelProperty(value = "腾讯云智服小程序唯一标识")
    private String yzfMpSign;

    @ApiModelProperty(value = "udesk IM标识")
    private String merchantEuid;

    @ApiModelProperty(value = "默认页面是否开启")
    private Boolean pageShow;

    public Store(Member member) {
        this.memberId = member.getId();
        this.memberName = member.getUsername();
        storeDisable = StoreStatusEnum.APPLY.value();
        selfOperated = false;
        deliveryScore = 5.0;
        serviceScore = 5.0;
        descriptionScore = 5.0;
        goodsNum = 0;
        collectionNum = 0;
    }

    public Store(Member member, AdminStoreApplyDTO adminStoreApplyDTO) {
        BeanUtil.copyProperties(adminStoreApplyDTO, this);

        this.memberId = member.getId();
        this.memberName = member.getUsername();
        storeDisable = StoreStatusEnum.APPLYING.value();
        selfOperated = false;
        deliveryScore = 5.0;
        serviceScore = 5.0;
        descriptionScore = 5.0;
        goodsNum = 0;
        collectionNum = 0;
    }

}