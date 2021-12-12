package cn.lili.modules.store.entity.vos;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 店铺流水下载
 *
 * @author Bulbasaur
 * @date: 2021/8/13 4:14 下午
 */
@Data
public class StoreFlowRefundDownloadVO extends StoreFlowPayDownloadVO {

    @ApiModelProperty(value = "售后SN")
    private String refundSn;

}
