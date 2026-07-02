package cn.lili.modules.finance.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 店铺流水导出查询参数。
 * <p>
 * 商家端由 {@code FinanceStoreController} 强制覆盖 {@code storeId}，防止越权查询他店数据。
 */
@Data
@EqualsAndHashCode(callSuper = true)
public class FinanceStoreFlowSearchParams extends PageVO {

    @Schema(description = "店铺ID")
    private String storeId;

    @Schema(description = "流水类型 PAY/REFUND")
    private String flowType;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "开始时间")
    private Date startTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "结束时间")
    private Date endTime;

    @Schema(description = "起始日期字符串")
    private String startDate;

    @Schema(description = "结束日期字符串")
    private String endDate;

    /** 是否已指定有效的时间筛选范围 */
    public boolean hasTimeRange() {
        return (startTime != null && endTime != null)
                || (CharSequenceUtil.isNotEmpty(startDate) && CharSequenceUtil.isNotEmpty(endDate));
    }
}
