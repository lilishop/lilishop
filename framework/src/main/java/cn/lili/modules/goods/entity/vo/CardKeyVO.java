package cn.lili.modules.goods.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.io.Serializable;
import java.util.Date;

/**
 * 卡密展示 VO（订单详情）
 *
 * @author Mike
 * @date 2026-07-31
 */
@Data
@Schema(description = "卡密信息")
public class CardKeyVO implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "卡号")
    private String cardNo;

    @Schema(description = "卡密明文")
    private String cardSecret;

    @Schema(description = "状态")
    private String status;

    @Schema(description = "发卡时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date allocatedTime;
}
