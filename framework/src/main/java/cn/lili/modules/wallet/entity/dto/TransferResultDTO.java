package cn.lili.modules.wallet.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
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

    @Schema(description = "错误信息")
    private String response;
    @Schema(description = "是否成功")
    private Boolean result;
    @Schema(description = "第三方转账单号")
    private String outBillNo;
    @Schema(description = "微信等待用户确认信息")
    private String wechatPackage;

}
