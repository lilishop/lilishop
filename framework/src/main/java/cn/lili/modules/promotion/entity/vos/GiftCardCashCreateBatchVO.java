package cn.lili.modules.promotion.entity.vos;

import cn.lili.modules.promotion.entity.dos.GiftCardCashCreateBatch;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.springframework.format.annotation.DateTimeFormat;

import java.math.BigDecimal;
import java.util.Date;

/**
 * 现金礼品卡制卡批次展示对象（含卡号范围、面额、过期时间等扩展字段）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Schema(description = "现金礼品卡制卡批次")
public class GiftCardCashCreateBatchVO extends GiftCardCashCreateBatch {

    @Schema(description = "制卡批次ID")
    private String createBatchId;

    @Schema(description = "卡号范围")
    private String cardNoRange;

    @Schema(description = "面值")
    private BigDecimal faceValue;

    @Schema(description = "礼品卡名称")
    private String giftCardName;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "过期时间")
    private Date expireTime;
}
