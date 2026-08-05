package cn.lili.modules.distribution.entity.vos;

import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 分销提现查询参数
 *
 * @author pikachu
 * @since 2020-03-26 09:04:53
 */
@Data
@EqualsAndHashCode(callSuper = false)
public class DistributionCashSearchParams extends PageVO {

    @Schema(description = "提现单编号")
    private String sn;

    @Schema(description = "分销员名称")
    private String memberName;

    @Schema(description = "提现状态", allowableValues = "APPLY,PASS,REFUSE")
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
