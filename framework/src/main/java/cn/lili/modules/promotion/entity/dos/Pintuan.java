package cn.lili.modules.promotion.entity.dos;

import cn.lili.modules.promotion.entity.dto.BasePromotion;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.validation.constraints.Min;

/**
 * 拼团活动实体类
 *
 * @author Chopper
 * @date 2020-03-19 10:44 上午
 */
@Data
@Entity
@Table(name = "li_pintuan")
@TableName("li_pintuan")
@ApiModel(value = "拼团")
public class Pintuan extends BasePromotion {


    private static final long serialVersionUID = -8465716592648602604L;


    @Min(message = "成团人数必须为数字", value = 0)
    @ApiModelProperty(value = "成团人数")
    private Integer requiredNum;

    @Min(message = "限购数量必须为数字", value = 0)
    @ApiModelProperty(value = "限购数量")
    private Integer limitNum;

    @ApiModelProperty(value = "虚拟成团", required = true)
    private Boolean fictitious;

    @ApiModelProperty(value = "拼团规则")
    private String pintuanRule;


}