package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销员端客户列表查询参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DistributionBuyerCustomerQueryDTO extends PageVO {

    /**
     * ALL / STARRED / ORDERED / EXPIRING_SOON
     */
    @Schema(description = "客户筛选类型")
    private String filterType = "ALL";

    /**
     * ALL / TODAY / YESTERDAY / LAST_7_DAYS / CUSTOM
     */
    @Schema(description = "时间范围类型")
    private String rangeType = "ALL";

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "自定义开始时间")
    private Date startTime;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "自定义结束时间")
    private Date endTime;

    @Schema(description = "手机号或昵称关键词")
    private String keyword;
}
