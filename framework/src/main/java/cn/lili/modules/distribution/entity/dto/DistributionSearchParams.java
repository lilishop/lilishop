package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.utils.StringUtils;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 分销查询参数
 *
 * @author Chopper
 * @since 2021/3/20 10:13
 */
@Data
public class DistributionSearchParams {

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "分销员状态", allowableValues = "APPLY,RETREAT,REFUSE,PASS")
    private String distributionStatus;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        queryWrapper.like(StringUtils.isNotEmpty(memberName), "member_name", memberName);
        queryWrapper.eq(StringUtils.isNotEmpty(distributionStatus), "distribution_status", distributionStatus);
        return queryWrapper;
    }
}
