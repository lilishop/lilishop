package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * 积分设置
 *
 * @author Chopper
 * @date 2020/11/17 7:59 下午
 */
@Data
public class PointSetting implements Serializable {

    private static final long serialVersionUID = -4261856614779031745L;
    @ApiModelProperty(value = "注册")
    private Integer register;

    @ApiModelProperty(value = "1元等级*积分")
    private Integer money;

    @ApiModelProperty(value = "每日签到积分")
    private Integer signIn;

    @ApiModelProperty(value = "订单评价赠送积分")
    private Integer comment;

    @ApiModelProperty(value = "积分具体设置")
    private List<PointSettingItem> pointSettingItems = new ArrayList<>();


}
