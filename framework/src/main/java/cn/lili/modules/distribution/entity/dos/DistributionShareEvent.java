package cn.lili.modules.distribution.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销商品分享行为记录
 *
 * @author pikachu
 */
@Data
@EqualsAndHashCode(callSuper = false)
@Schema(description = "分销商品分享行为记录")
@TableName("li_distribution_share_event")
@NoArgsConstructor
public class DistributionShareEvent extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @Schema(description = "客户端事件ID（幂等）")
    private String eventId;

    @Schema(description = "分享标识（用于访问归因）")
    private String shareId;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "商品ID")
    private String goodsId;

    @Schema(description = "SKU ID")
    private String skuId;

    @Schema(description = "分享渠道")
    private String channel;

    @Schema(description = "触发页面")
    private String triggerPage;

    @Schema(description = "分享按钮类型")
    private String shareButtonType;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "分享时间")
    private Date shareTime;
}
