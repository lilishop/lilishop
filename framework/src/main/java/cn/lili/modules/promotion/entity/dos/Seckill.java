package cn.lili.modules.promotion.entity.dos;

import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.promotion.entity.dto.BasePromotion;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.NotNull;
import java.util.Date;

/**
 * 秒杀活动实体类
 *
 * @author Chopper
 * @date 2020-03-19 10:44 上午
 */
@Data
@Entity
@Table(name = "li_seckill")
@TableName("li_seckill")
@ApiModel(value = "秒杀活动活动")
@NoArgsConstructor
public class Seckill extends BasePromotion {

    private static final long serialVersionUID = -9116425737163730836L;

    @NotNull(message = "请填写报名截止时间")
    @ApiModelProperty(value = "报名截至时间", required = true)
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    private Date applyEndTime;

    @ApiModelProperty(value = "申请规则")
    private String seckillRule;

    @ApiModelProperty(value = "开启几点场 例如：6，8，12")
    @NotNull(message = "活动时间段不能为空")
    private String hours;

    /**
     * 已参与此活动的商家id集合
     */
    @ApiModelProperty(value = "商家id集合以逗号分隔")
    private String storeIds;

    @ApiModelProperty(value = "商品数量")
    private Integer goodsNum;

    public Seckill(int day,String hours,String seckillRule){
        //默认创建*天后的秒杀活动
        DateTime dateTime= DateUtil.beginOfDay(DateUtil.offset(new DateTime(), DateField.DAY_OF_YEAR, day));
        this.applyEndTime=dateTime;
        this.hours=hours;
        this.seckillRule=seckillRule;
        this.goodsNum=0;

        //BasePromotion
        setStoreName("platform");
        setStoreId("platform");
        setPromotionName(DateUtil.formatDate(dateTime)+" 秒杀活动");
        setStartTime(dateTime);
        setEndTime(DateUtil.endOfDay(dateTime));
        setPromotionStatus(PromotionStatusEnum.NEW.name());

    }
}