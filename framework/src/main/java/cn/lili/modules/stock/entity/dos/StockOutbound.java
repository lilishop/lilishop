package cn.lili.modules.stock.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 商家出库单实体类
 * 用于记录非平台交易订单（如线下销售、借出、赠品、调拨等）的手工出库单据与审批流
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Data
@EqualsAndHashCode(callSuper = false)
@TableName("li_stock_outbound")
@Schema(description = "商家出库单")
public class StockOutbound extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "出库单号")
    private String outboundSn;

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "店铺名称")
    private String storeName;

    @Schema(description = "单据状态")
    private String status;

    @Schema(description = "出库原因ID")
    private String reasonId;

    @Schema(description = "出库原因名称")
    private String reasonName;

    @Schema(description = "出库总数量")
    private Integer totalQuantity;

    @Schema(description = "备注")
    private String remark;

    @Schema(description = "审核人")
    private String auditBy;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "审核时间")
    private Date auditTime;

    @Schema(description = "审核备注")
    private String auditRemark;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "完成时间")
    private Date completeTime;
}
