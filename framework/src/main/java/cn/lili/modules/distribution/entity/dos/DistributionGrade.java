package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;

/**
 * 分销等级。
 * <p>
 * 升级规则：对某一等级已启用的销售额、收益额、邀请人数条件须<strong>全部</strong>达标方可升级。
 * </p>
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销等级")
@TableName("li_distribution_grade")
@NoArgsConstructor
public class DistributionGrade extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "等级名称")
    private String gradeName;

    @Schema(description = "等级值（1-5，越小等级越低）")
    @TableField("grade_sort")
    private Integer sortOrder;

    @Schema(description = "升级销售额阈值")
    private BigDecimal salesThreshold;

    @Schema(description = "升级收益额阈值")
    private BigDecimal commissionThreshold;

    @Schema(description = "升级邀请人数阈值")
    private Integer inviteThreshold;

    @Schema(description = "是否启用销售额升级条件")
    @TableField("sales_enabled")
    private Boolean enableSalesCondition;

    @Schema(description = "是否启用收益额升级条件")
    @TableField("commission_enabled")
    private Boolean enableCommissionCondition;

    @Schema(description = "是否启用邀请人数升级条件")
    @TableField("invite_enabled")
    private Boolean enableInviteCondition;

    @Schema(description = "商品佣金比例（0～1，0.1 表示 10%）")
    @TableField("product_commission_rate")
    private BigDecimal goodsCommissionRate;

    @Schema(description = "邀请佣金比例（0～1，0.1 表示 10%）")
    private BigDecimal inviteCommissionRate;
}
