package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.utils.StringUtils;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销客户关系查询参数
 *
 * @author pikachu
 */
@Data
public class DistributionCustomerRelationSearchParams {

    @Schema(description = "客户会员ID")
    private String memberId;

    @Schema(description = "分销员ID")
    private String distributionId;

    @Schema(description = "客户手机号")
    private String memberMobile;

    @Schema(description = "分销员手机号")
    private String distributionMobile;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "绑定时间开始")
    private Date bindTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "绑定时间结束")
    private Date bindTimeEnd;

    @Schema(description = "当前关系是否有效")
    private Boolean relationEffective;

    @Schema(description = "是否有截止日期")
    private Boolean hasDeadline;

    @Schema(description = "关系状态")
    private String relationStatus;

    public LambdaQueryWrapper<DistributionCustomerRelation> queryWrapper() {
        LambdaQueryWrapper<DistributionCustomerRelation> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(StringUtils.isNotEmpty(memberId), DistributionCustomerRelation::getMemberId, memberId);
        queryWrapper.eq(StringUtils.isNotEmpty(distributionId), DistributionCustomerRelation::getDistributionId, distributionId);
        queryWrapper.eq(StringUtils.isNotEmpty(relationStatus), DistributionCustomerRelation::getRelationStatus, relationStatus);
        queryWrapper.orderByDesc(DistributionCustomerRelation::getBindTime);
        return queryWrapper;
    }
}
