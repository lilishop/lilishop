package cn.lili.modules.member.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 会员分布VO
 *
 * @author Chopper
 * @since 2021-02-26 17:25
 */
@Data
public class MemberDistributionVO {

    @ApiModelProperty(value = "客户端类型")
    private String clientEnum;

    @ApiModelProperty(value = "数量")
    private Integer num;

    @ApiModelProperty(value = "比例")
    private Double proportion;

}
