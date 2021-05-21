package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.dto.BasePromotion;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

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
@Table(name = "li_coupon_activity")
@TableName("li_coupon_activity")
@ApiModel(value = "优惠券活动实体类")
public class CouponActivity extends BasePromotion {

    @ApiModelProperty(value = "优惠券活动类型")
    private String couponActivityType;

    @ApiModelProperty(value = "活动范围", allowableValues = "ALL:全部会员,DESIGNATED：指定会员")
    private String activityScope;

    @ApiModelProperty(value = "活动范围")
    private String activityScopeInfo;

}