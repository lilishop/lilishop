package cn.lili.modules.distribution.entity.vos;

import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyFormItemSnapshot;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;

/**
 * 分销员详情 VO
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
public class DistributionMemberDetailVO {

    @Schema(description = "分销员ID")
    private String id;

    @Schema(description = "会员ID")
    private String memberId;

    @Schema(description = "手机号")
    private String mobile;

    @Schema(description = "昵称")
    private String nickname;

    @Schema(description = "用户名")
    private String username;

    @Schema(description = "头像")
    private String face;

    @Schema(description = "性别：0保密 1男 2女")
    private Integer sex;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "客户注册时间")
    private Date memberCreateTime;

    @Schema(description = "等级名称")
    private String gradeName;

    @Schema(description = "分销员状态")
    private String distributionStatus;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "加入时间")
    private Date joinTime;

    @Schema(description = "待审核申请ID")
    private String pendingApplicationId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "申请提交时间")
    private Date applyTime;

    @Schema(description = "申请来源：MANUAL_ENTRY AUTO_ENTRY ADMIN")
    private String applySource;

    @Schema(description = "申请时填写的动态表单快照")
    private List<DistributionRecruitApplyFormItemSnapshot> applyFormItems;

    @Schema(description = "累计销售额")
    private BigDecimal validSalesAmount;

    @Schema(description = "累计订单数")
    private Integer orderCount;

    @Schema(description = "累计客户数")
    private Integer totalCustomerCount;

    @Schema(description = "有效客户数")
    private Integer validCustomerCount;

    @Schema(description = "累计邀请数")
    private Integer inviteCount;

    @Schema(description = "累计佣金")
    private BigDecimal totalCommission;

    @Schema(description = "商品佣金")
    private BigDecimal goodsCommission;

    @Schema(description = "邀请佣金")
    private BigDecimal inviteCommission;

    @Schema(description = "推广金额")
    private BigDecimal promotionAmount;

    @Schema(description = "自购金额")
    private BigDecimal selfPurchaseAmount;
}
