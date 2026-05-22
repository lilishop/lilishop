package cn.lili.modules.promotion.entity.dto.search;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 发卡/发送记录分页查询条件
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "发卡/发送记录分页查询")
public class GiftCardCashIssueBatchSearchParams {

    @Schema(description = "活动ID")
    private String activityId;

    @Schema(description = "制卡批次ID（可选，筛选从某批发放的记录）")
    private String createBatchId;

    @Schema(description = "发卡批次ID")
    private String issueBatchId;

    @Schema(description = "卡号（模糊）")
    private String cardNo;

    @Schema(description = "发卡人（支持姓名/手机号模糊）")
    private String issueUser;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "发卡时间开始")
    private Date issueTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "发卡时间结束")
    private Date issueTimeEnd;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "过期时间开始")
    private Date expireTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "过期时间结束")
    private Date expireTimeEnd;

}
