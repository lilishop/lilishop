package cn.lili.modules.promotion.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 优惠券活动实体类
 *
 * @author Bulbasaur
 * @date 2020-03-19 10:44 上午
 */
@Data
@Entity
@Table(name = "li_coupon_activity_item")
@TableName("li_coupon_activity_item")
@ApiModel(value = "优惠券活动-优惠券关联实体类")
public class CouponActivityItem extends BaseEntity {

    @ApiModelProperty(value = "优惠券活动ID")
    private String activityId;

    @ApiModelProperty(value = "优惠券ID")
    private String couponId;

    @ApiModelProperty(value = "优惠券数量")
    private Integer num;


}