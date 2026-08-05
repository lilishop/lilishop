package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.utils.StringUtils;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销查询参数
 *
 * @author Chopper
 * @since 2021/3/20 10:13
 */
@Data
public class DistributionSearchParams {

    @Schema(description = "分销员ID")
    private String id;

    @Schema(description = "客户名称")
    private String memberName;

    @Schema(description = "分销员手机号")
    private String mobile;

    @Schema(description = "昵称")
    private String nickname;

    @Schema(description = "邀请人手机号")
    private String inviterMobile;

    @Schema(description = "分销等级ID")
    private String gradeId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "加入时间开始")
    private Date joinTimeStart;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "加入时间结束")
    private Date joinTimeEnd;

    @Schema(description = "分销员状态", allowableValues = "APPLY,RETREAT,REFUSE,PASS")
    private String distributionStatus;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        queryWrapper.like(StringUtils.isNotEmpty(memberName), "member_name", memberName);
        queryWrapper.eq(StringUtils.isNotEmpty(distributionStatus), "distribution_status", distributionStatus);
        return queryWrapper;
    }
}
