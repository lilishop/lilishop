package cn.lili.modules.promotion.entity.dto.search;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 制卡批次分页查询条件
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@Schema(description = "制卡批次查询")
public class GiftCardCashCreateBatchSearchParams {

    @Schema(description = "活动ID")
    private String activityId;

    @Schema(description = "本批备注（模糊）")
    private String batchRemark;

    @Schema(description = "卡号（模糊）")
    private String cardNo;

    @Schema(description = "制卡批次ID")
    private String createBatchId;

    @Schema(description = "制卡人名称（模糊）")
    private String createUserName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "制卡时间开始")
    private Date createTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "制卡时间结束")
    private Date createTimeEnd;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "过期时间开始")
    private Date expireTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "过期时间结束")
    private Date expireTimeEnd;
}