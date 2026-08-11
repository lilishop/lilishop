package cn.lili.modules.promotion.entity.dto;


import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Min;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * 砍价活动商品操作DTO
 *
 * @author qiuqiu
 * @date 2020/8/21
 **/
@Data
public class KanjiaActivityGoodsOperationDTO implements Serializable {


    private static final long serialVersionUID = -1378599087650538592L;

    @Min(value = 0, message = "活动开始时间不能为空")
    @Schema(description = "活动开始时间", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Min(value = 0, message = "活动结束时间不能为空")
    @Schema(description = "活动结束时间", requiredMode = Schema.RequiredMode.REQUIRED)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

    @Schema(description = "砍价活动商品列表")
    List<KanjiaActivityGoodsDTO> promotionGoodsList;

}
