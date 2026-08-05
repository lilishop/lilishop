package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销员推广/邀请订单查询参数
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DistributionBuyerOrderQueryDTO extends PageVO {

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

    @Schema(description = "搜索关键词：手机号、订单号、商品名称")
    private String keyword;

    /**
     * PROMOTION / INVITE
     */
    @Schema(description = "订单类型：推广订单/邀请订单")
    private String orderType = "PROMOTION";
}
