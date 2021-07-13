package cn.lili.modules.promotion.entity.dos;

import cn.lili.base.BaseEntity;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.util.Date;

/**
 * 砍价活动商品实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@Entity
@Table(name = "li_kanjia_activity_goods")
@TableName("li_kanjia_activity_goods")
@ApiModel(value = "砍价活动商品对象")
public class KanjiaActivityGoods extends BaseEntity {

    private static final long serialVersionUID = 6694714877345423488L;

    @ApiModelProperty(value = "结算价格")
    @NotEmpty(message = "结算价格不能为空")
    private Double settlementPrice;

    @ApiModelProperty(value = "最低购买金额")
    @NotEmpty(message = "最低购买金额不能为空")
    private Double purchasePrice;

    @ApiModelProperty(value = "货品id")
    @NotEmpty(message = "货品id不能为空")
    private String skuId;

    @ApiModelProperty(value = "货品名称")
    private String goodsName;

    @ApiModelProperty(value = "缩略图")
    private String thumbnail;

    @ApiModelProperty(value = "活动库存")
    @NotEmpty(message = "活动库存不能为空")
    private Integer stock;

    @ApiModelProperty(value = "每人最低砍价金额")
    @NotEmpty(message = "每人最低砍价金额不能为空")
    private Double lowestPrice;

    @ApiModelProperty(value = "每人最高砍价金额")
    @NotEmpty(message = "每人最高砍价金额不能为空")
    private Double highestPrice;

    /**
     * @see PromotionStatusEnum
     */
    @ApiModelProperty(value = "活动状态")
    @NotNull(message = "活动状态不能为空")
    private String promotionStatus;

    @Min(message = "活动开始时间不能为空", value = 0)
    @ApiModelProperty(value = "活动开始时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date startTime;

    @Min(message = "活动结束时间不能为空", value = 0)
    @ApiModelProperty(value = "活动结束时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date endTime;



}