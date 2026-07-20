package cn.lili.modules.statistics.entity.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 销售报表查询参数
 *
 * @author Bulbasaur
 * @since 2026/07/20
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class SalesReportQueryParam extends StatisticsQueryParam {

    @Schema(description = "统计日期类型 DAY|PERIOD")
    private String reportDateType;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "固定日期(按日统计)")
    private Date queryDate;

    @Schema(description = "分类ID")
    private String categoryId;

    @Schema(description = "品牌ID")
    private String brandId;

    @Schema(description = "订单来源")
    private String clientType;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "商品名称关键词")
    private String keyword;

    @Schema(description = "订单编号")
    private String orderSn;

    @Schema(description = "买家名称")
    private String memberName;

    @Schema(description = "订单状态")
    private String orderStatus;

    @Schema(description = "支付状态")
    private String payStatus;

    @Schema(description = "支付方式")
    private String paymentMethod;

    @Schema(description = "排序维度 NUM|PRICE")
    private String sortType;

    @Schema(description = "店铺业绩是否按日分组(商家端)")
    private Boolean groupByDay;
}
