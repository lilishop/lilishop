package cn.lili.modules.promotion.entity.dto;

import cn.lili.base.BaseEntity;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.MappedSuperclass;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Date;

/**
 * 促销活动基础类
 *
 * @author Chopper
 * @date 2020-03-19 10:44 上午
 */
@Data
@MappedSuperclass
public class BasePromotion extends BaseEntity {

    private static final long serialVersionUID = 7814832369110695758L;

    @ApiModelProperty(value = "商家名称，如果是平台，这个值为 platform")
    private String storeName;

    @ApiModelProperty(value = "商家id，如果是平台，这个值为 platform")
    private String storeId;

    @NotEmpty(message = "活动名称不能为空")
    @ApiModelProperty(value = "活动名称", required = true)
    private String promotionName;

    @Min(message = "活动开始时间不能为空", value = 0)
    @ApiModelProperty(value = "活动开始时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Min(message = "活动结束时间不能为空", value = 0)
    @ApiModelProperty(value = "活动结束时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

    /**
     * @see PromotionStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    @NotNull(message = "活动状态不能为空")
    private String promotionStatus;
}
