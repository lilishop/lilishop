package cn.lili.modules.promotion.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 优惠券礼包详情展示VO
 * 用于买家端展示券礼包基本信息、总价值、包含优惠券及领取状态
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@Schema(description = "优惠券礼包详情展示VO")
public class CouponPackageVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "券礼包活动ID")
    private String id;

    @Schema(description = "券礼包活动名称")
    private String activityName;

    @Schema(description = "活动开始时间")
    private Date startTime;

    @Schema(description = "活动结束时间")
    private Date endTime;

    @Schema(description = "活动状态")
    private String promotionStatus;

    @Schema(description = "券礼包宣传总面额，单位：元")
    private Double totalAmount;

    @Schema(description = "当前登录用户是否已领取：true已领取，false未领取")
    private Boolean hasReceived;

    @Schema(description = "优惠券子项列表")
    private List<CouponActivityItemVO> couponItems;
}
