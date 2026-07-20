package cn.lili.modules.statistics.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 会员新增人数趋势数据
 *
 * @author Bulbasaur
 * @since 2026/07/17
 */
@Data
public class MemberNewTrendVO {

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    @Schema(description = "日期")
    private Date date;

    @Schema(description = "新增会员数")
    private Long newlyAdded = 0L;
}
