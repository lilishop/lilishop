package cn.lili.modules.distribution.entity.vos;

import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 分销员对象
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Data
@ApiModel(value = "分销订单查询对象")
public class DistributionOrderSearchParams extends PageVO {

    private static final long serialVersionUID = -8736018687663645064L;

    @ApiModelProperty(value = "分销员名称")
    private String distributionName;

    @ApiModelProperty(value = "订单sn")
    private String orderSn;

    @ApiModelProperty(value = "分销员ID", hidden = true)
    private String distributionId;

    @ApiModelProperty(value = "分销订单状态")
    private String distributionOrderStatus;

    @ApiModelProperty(value = "店铺ID")
    private String storeId;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "开始时间")
    private Date startTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "结束时间")
    private Date endTime;


    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = Wrappers.query();
        queryWrapper.like(StringUtils.isNotBlank(distributionName), "distribution_name", distributionName);
        queryWrapper.eq(StringUtils.isNotBlank(distributionOrderStatus), "distribution_order_status", distributionOrderStatus);
        queryWrapper.eq(StringUtils.isNotBlank(orderSn), "order_sn", orderSn);
        queryWrapper.eq(StringUtils.isNotBlank(distributionId), "distribution_id", distributionId);
        queryWrapper.eq(StringUtils.isNotBlank(storeId), "store_id", storeId);
        if (endTime != null && startTime != null) {
            queryWrapper.between("create_time", startTime, endTime);
        }
        return queryWrapper;
    }

}