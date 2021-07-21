package cn.lili.modules.distribution.entity.vos;

import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 分销佣金查询信息
 *
 * @author pikachu
 * @since 2020-03-26 09:04:53
 */
@Data
public class DistributionCashSearchParams extends PageVO {

    /**
     * 编号
     */
    @ApiModelProperty(value = "编号")
    private String sn;
    /**
     * 会员名称
     */
    @ApiModelProperty(value = "会员名称")
    private String memberName;

    /**
     * 分销员提现状态
     */
    @ApiModelProperty(value = "分销员提现状态",allowableValues = "APPLY,PASS,REFUSE")
    private String distributionCashStatus;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (StringUtils.isNotEmpty(memberName)) {
            queryWrapper.like("distribution_name", memberName);
        }
        if (StringUtils.isNotEmpty(sn)) {
            queryWrapper.like("sn", sn);
        }
        if (StringUtils.isNotEmpty(distributionCashStatus)) {
            queryWrapper.like("distribution_cash_status", distributionCashStatus);
        }
        return queryWrapper;
    }


}
