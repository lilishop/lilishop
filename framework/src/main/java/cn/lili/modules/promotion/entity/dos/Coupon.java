package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.dto.BasePromotion;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 优惠券活动实体类
 *
 * @author Chopper
 * @date 2020-03-19 10:44 上午
 */
@Data
@Entity
@Table(name = "li_coupon")
@TableName("li_coupon")
@ApiModel(value = "优惠券实体类")
public class Coupon extends BasePromotion {


    private static final long serialVersionUID = 8372820376262437018L;


    @ApiModelProperty(value = "优惠券名称")
    private String couponName;

    /**
     * POINT("打折"), PRICE("减免现金");
     *
     * @see cn.lili.modules.promotion.entity.enums.CouponTypeEnum
     */
    @ApiModelProperty(value = "优惠券类型")
    private String couponType;

    /**
     * @see cn.lili.modules.promotion.entity.enums.CouponScopeTypeEnum
     */
    @ApiModelProperty(value = "关联范围类型")
    private String scopeType;

    @ApiModelProperty(value = "面额")
    private Double price;

    @ApiModelProperty(value = "折扣")
    private Double couponDiscount;

    @ApiModelProperty(value = "范围关联的id")
    @Column(columnDefinition = "TEXT")
    private String scopeId;

    /**
     * @see cn.lili.modules.promotion.entity.enums.CouponGetEnum
     */
    @ApiModelProperty(value = "优惠券类型，分为免费领取和活动赠送")
    private String getType;

    @ApiModelProperty(value = "店铺承担比例,平台发布时可以提供一定返点")
    private Double storeCommission;

    @ApiModelProperty(value = "活动描述")
    private String description;

    @ApiModelProperty(value = "发行数量,如果是0则是不限制")
    private Integer publishNum;

    @ApiModelProperty(value = "领取限制")
    private Integer couponLimitNum;

    @ApiModelProperty(value = "已被使用的数量")
    private Integer usedNum;

    @ApiModelProperty(value = "已被领取的数量")
    private Integer receivedNum;

    @ApiModelProperty(value = "消费门槛")
    private Double consumeThreshold;

    /**
     * @see cn.lili.modules.promotion.entity.enums.CouponRangeDayEnum
     *
     */
    @ApiModelProperty(value = "时间范围类型")
    private String rangeDayType;

    @ApiModelProperty(value = "有效期")
    private Integer effectiveDays;

}