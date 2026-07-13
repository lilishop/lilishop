package cn.lili.modules.promotion.entity.dos;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;
import org.springframework.beans.BeanUtils;

import java.util.Date;

/**
 * 秒杀活动实体类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_seckill")
@Schema(description = "秒杀活动活动")
@NoArgsConstructor
@ToString(callSuper = true)
public class Seckill extends BasePromotions {

    private static final long serialVersionUID = -9116425737163730836L;

    @NotNull(message = "请填写报名截止时间")
    @Schema(description = "报名截至时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date applyEndTime;

    @Schema(description = "申请规则")
    private String seckillRule;

    @Schema(description = "开启几点场 例如：6，8，12")
    @NotNull(message = "活动时间段不能为空")
    private String hours;

    /**
     * 已参与此活动的商家id集合
     */
    @Schema(description = "商家id集合以逗号分隔")
    private String storeIds;

    @Schema(description = "商品数量")
    private Integer goodsNum;

    public Seckill(int day, String hours, String seckillRule) {
        //默认创建*天后的秒杀活动
        DateTime dateTime = DateUtil.beginOfDay(DateUtil.offsetDay(new Date(), day));
        this.applyEndTime = dateTime;
        this.hours = hours;
        this.seckillRule = seckillRule;
        this.goodsNum = 0;
        //BasePromotion
        this.setStoreName(PromotionTools.PLATFORM_NAME);
        this.setStoreId(PromotionTools.PLATFORM_ID);
        this.setPromotionName(DateUtil.formatDate(dateTime) + " 秒杀活动");
        this.setStartTime(dateTime);
        this.setEndTime(DateUtil.endOfDay(dateTime));
    }

    public Seckill(SeckillVO seckillVO) {
        BeanUtils.copyProperties(seckillVO, this);
    }
}