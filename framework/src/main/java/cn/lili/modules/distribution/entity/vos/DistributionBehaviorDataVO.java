package cn.lili.modules.distribution.entity.vos;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.math.BigDecimal;

/**
 * 分销员行为数据VO
 *
 * @author pikachu
 */
@Data
public class DistributionBehaviorDataVO {

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "用户ID")
    private String memberId;

    @Schema(description = "用户昵称")
    private String nickname;

    @Schema(description = "手机号")
    private String mobile;

    @Schema(description = "等级名称")
    private String gradeName;

    @Schema(description = "新增销售额")
    private BigDecimal newSalesAmount;

    @Schema(description = "新增客户数")
    private Long newCustomerCount;

    @Schema(description = "支付客户数")
    private Long paidCustomerCount;

    @Schema(description = "新增邀请数")
    private Long newInviteCount;

    @Schema(description = "开单邀请数")
    private Long orderInviteCount;

    @Schema(description = "商品分享数")
    private Long shareCount;

    @Schema(description = "分享商品数")
    private Long sharedGoodsCount;

    @Schema(description = "商品分享引流")
    private Long shareVisitorCount;

    @Schema(description = "商品分享浏览量")
    private Long shareVisitCount;
}
