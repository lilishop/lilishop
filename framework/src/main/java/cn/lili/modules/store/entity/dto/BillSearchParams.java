package cn.lili.modules.store.entity.dto;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.store.entity.enums.BillStatusEnum;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

/**
 * 结算单搜索参数
 *
 * @author Chopper
 * @since 2021/3/17 6:08 下午
 */
@Data
public class BillSearchParams extends PageVO {

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "起始日期")
    private String startDate;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "结束日期")
    private String endDate;

    @ApiModelProperty(value = "账单号")
    private String sn;

    /**
     * @see BillStatusEnum
     */
    @ApiModelProperty(value = "状态：OUT(已出账),CHECK(已对账),EXAMINE(已审核),PAY(已付款)")
    private String billStatus;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "店铺ID", hidden = true)
    private String storeId;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> wrapper = new QueryWrapper<>();

        //创建时间
        if (StringUtils.isNotEmpty(startDate) && StringUtils.isNotEmpty(endDate)) {
            wrapper.between("create_time", startDate, endDate);
        } else if (StringUtils.isNotEmpty(startDate)) {
            wrapper.ge("create_time", startDate);
        } else if (StringUtils.isNotEmpty(endDate)) {
            wrapper.le("create_time", endDate);
        }
        //账单号
        wrapper.eq(StringUtils.isNotEmpty(sn), "sn", sn);
        //结算状态
        wrapper.eq(StringUtils.isNotEmpty(billStatus), "bill_status", billStatus);
        //店铺名称
        wrapper.eq(StringUtils.isNotEmpty(storeName), "store_name", storeName);
        //按卖家查询
        wrapper.eq(StringUtils.equals(UserContext.getCurrentUser().getRole().name(), UserEnums.STORE.name()),
                "store_id", UserContext.getCurrentUser().getStoreId());
        return wrapper;
    }

}
