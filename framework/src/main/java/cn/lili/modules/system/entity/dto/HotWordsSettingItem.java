package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 积分签到设置
 *
 * @author Chopper
 * @since 2021-02-26 11:48
 */
@Data
public class HotWordsSettingItem implements Comparable<HotWordsSettingItem>, Serializable {


    @ApiModelProperty(value = "热词")
    private String keywords;


    @ApiModelProperty(value = "默认分数")
    private Integer score;


    public Integer getScore() {
        if (score == null || score < 0) {
            return 0;
        }
        return score;
    }

    public void setScore(Integer score) {
        this.score = score;
    }

    @Override
    public int compareTo(HotWordsSettingItem pointSettingItem) {
        return pointSettingItem.getScore();
    }
}
