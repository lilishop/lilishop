package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 积分签到设置
 *
 * @author Chopper
 * @date 2021-02-26 11:48
 */
@Data
public class PointSettingItem implements Comparable<PointSettingItem> {


    @ApiModelProperty(value = "签到天数")
    private Integer day;


    @ApiModelProperty(value = "赠送积分")
    private Integer point;

    @Override
    public int compareTo(PointSettingItem pointSettingItem) {
        return this.day - pointSettingItem.getDay();
    }
}
