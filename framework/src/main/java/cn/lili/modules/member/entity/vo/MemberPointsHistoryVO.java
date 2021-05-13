package cn.lili.modules.member.entity.vo;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 会员积分VO
 *
 * @author Chopper
 * @date 2021/2/25 9:52 上午
 */
@Data
public class MemberPointsHistoryVO {

    @ApiModelProperty(value = "积分总数")
    private Long point;

    @ApiModelProperty(value = "未使用积分总数")
    private Long variablePoint;


}
