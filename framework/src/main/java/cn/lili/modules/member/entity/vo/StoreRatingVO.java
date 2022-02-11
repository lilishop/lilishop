package cn.lili.modules.member.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 评分VO
 *
 * @author Chopper
 * @since 2021/3/15 5:55 下午
 */
@Data
public class StoreRatingVO {

    @ApiModelProperty(value = "物流评分")
    private String deliveryScore;

    @ApiModelProperty(value = "服务评分")
    private String serviceScore;

    @ApiModelProperty(value = "描述评分")
    private String descriptionScore;

}
