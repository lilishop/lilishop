package cn.lili.modules.procurement.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_damage_report")
/**
 * 报损单实体，记录店铺报损的基础信息与汇总数据
 * @author Bulbasaur
 * @since 2025-12-18
 */
public class DamageReport extends BaseEntity {

    @Schema(description = "报损单号")
    private String sn;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "状态")
    private String status;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "报损日期")
    private Date damageDate;

    @Schema(description = "报损原因ID")
    private String damageReasonId;

    @Schema(description = "备注")
    private String remark;

    @Schema(description = "凭证图片")
    private String evidence;

    @Schema(description = "报损总数量")
    private Integer totalQuantity;

    @Schema(description = "报损总金额")
    private Double totalAmount;

    @Schema(description = "制单人ID")
    private String makerId;

    @Schema(description = "制单人")
    private String makerName;

    @Schema(description = "审核时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date auditTime;

    @Schema(description = "审核人ID")
    private String auditorId;

    @Schema(description = "审核人")
    private String auditorName;
}
