package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * 秒杀活动设置
 *
 * @author Bulbasaur
 * @since 2021/5/24 3:27 下午
 */
@Data
public class SeckillSetting implements Serializable {

    @ApiModelProperty(value = "开启几点场 例如：6,8,12")
    @NotNull(message = "活动时间段不能为空")
    private String hours;

    @ApiModelProperty(value = "秒杀规则")
    @NotNull(message = "秒杀规则不能为空")
    private String seckillRule;
}
