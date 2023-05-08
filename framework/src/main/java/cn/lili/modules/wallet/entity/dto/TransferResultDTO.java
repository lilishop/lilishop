package cn.lili.modules.wallet.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Builder;
import lombok.Data;

/**
 * 转账结果
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2023/5/6 16:10
 */
@Data
@Builder
public class TransferResultDTO {

    @ApiModelProperty(value = "错误信息")
    private String response;
    @ApiModelProperty(value = "是否成功")
    private Boolean result;

}
