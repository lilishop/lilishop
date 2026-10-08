package cn.lili.modules.order.order.entity.dto;

import cn.lili.common.vo.PageVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 核销记录查询
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "核销记录查询")
public class OrderVerificationRecordSearchParams extends PageVO {

    @Schema(description = "商品名称")
    private String goodsName;

    @Schema(description = "核销员")
    private String verifierName;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "核销开始时间")
    private Date startTime;

    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "核销结束时间")
    private Date endTime;
}
