package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 经验值设置
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:10 下午
 */
@Data
public class ExperienceSetting implements Serializable {

    private static final long serialVersionUID = -4261856614779031745L;
    @ApiModelProperty(value = "注册")
    private Integer register;

    @ApiModelProperty(value = "每日签到经验值")
    private Integer signIn;

    @ApiModelProperty(value = "订单评价赠送经验值")
    private Integer comment;

    @ApiModelProperty(value = "分享获取经验值")
    private Integer share;

    @ApiModelProperty(value = "购物获取经验值,一元*经验值")
    private Integer money;

}
