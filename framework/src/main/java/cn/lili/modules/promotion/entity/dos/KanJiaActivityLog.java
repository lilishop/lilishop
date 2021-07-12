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
@Table(name = "li_kanjia_activity_log")
@TableName("li_kanjia_activity_log")
@ApiModel(value = "砍价活动日志对象")
public class KanJiaActivityLog extends BaseEntity {


    private static final long serialVersionUID = 3977352717995562783L;

    @ApiModelProperty(value = "砍价活动参与记录id")
    private String kanJiaActivityId;

    @ApiModelProperty(value = "砍价会员id")
    private String kanJiaMemberId;

    @ApiModelProperty(value = "砍价会员名称")
    private String kanJiaMemberName;

    @ApiModelProperty(value = "砍价会员头像")
    private String kanJiaMemberFace;

    @ApiModelProperty(value = "砍价金额")
    private Double kanJiaPrice;

    @ApiModelProperty(value = "剩余购买金额")
    private Double surplusPrice;


}