package cn.lili.modules.store.entity.vos;

import cn.lili.modules.store.entity.enums.BillStatusEnum;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.util.Date;

/**
 * 结算单VO
 *
 * @author pikachu
 * @since 2020年3月07日 上午11:04:25
 */
@Data
public class BillListVO {

    @ApiModelProperty(value = "账单ID")
    private String id;

    @ApiModelProperty(value = "账单号")
    private String sn;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "结算开始时间")
    private Date startTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "结算结束时间")
    private Date endTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    @ApiModelProperty(value = "出账时间")
    private Date createTime;

    /**
     * @see BillStatusEnum
     */
    @ApiModelProperty(value = "状态：OUT(已出账),RECON(已对账),PASS(已审核),PAY(已付款)")
    private String billStatus;

    @ApiModelProperty(value = "店铺名称")
    private String storeName;

    @ApiModelProperty(value = "最终结算金额")
    private Double billPrice;
}
