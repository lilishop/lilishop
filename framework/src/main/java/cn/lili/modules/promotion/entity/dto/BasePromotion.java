package cn.lili.modules.promotion.entity.dto;

import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.mybatis.BaseEntity;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import javax.validation.constraints.NotEmpty;
import java.util.Date;

/**
 * 促销活动基础类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
public class BasePromotion extends BaseEntity {

    private static final long serialVersionUID = 7814832369110695758L;

    @ApiModelProperty(value = "商家名称，如果是平台，这个值为 platform")
    private String storeName;

    @ApiModelProperty(value = "商家id，如果是平台，这个值为 platform")
    private String storeId;

    @NotEmpty(message = "活动名称不能为空")
    @ApiModelProperty(value = "活动名称", required = true)
    private String promotionName;

    @ApiModelProperty(value = "活动开始时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @ApiModelProperty(value = "活动结束时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;

    /**
     * @see PromotionStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    private String promotionStatus;
}
