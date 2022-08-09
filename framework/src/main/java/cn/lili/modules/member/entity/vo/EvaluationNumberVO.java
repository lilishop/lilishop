package cn.lili.modules.member.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 评价数量VO
 *
 * @author Chopper
 * @since 2021/1/27 10:41 上午
 */
@Data
public class EvaluationNumberVO {

    @ApiModelProperty(value = "全部评价")
    private Integer all;

    @ApiModelProperty(value = "好评数量")
    private Integer good;

    @ApiModelProperty(value = "中评数量")
    private Integer moderate;

    @ApiModelProperty(value = "差评数量")
    private Integer worse;

    @ApiModelProperty(value = "有图数量")
    private Long haveImage;
}
