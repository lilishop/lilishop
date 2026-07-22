package cn.lili.modules.procurement.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.util.Date;

/**
 * 采购单实体
 * 记录采购单的基础信息与状态
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_procurement_order")
public class ProcurementOrder extends BaseEntity {

    @Schema(description = "采购订单编号")
    private String orderSn;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "采购总金额")
    private Double totalAmount;

    @Schema(description = "采购总数量")
    private Integer totalQuantity;

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

    @Schema(description = "备注")
    private String remark;

    @Schema(description = "出入库原因ID")
    private String stockReasonId;

    @Schema(description = "状态")
    private String status;
}
