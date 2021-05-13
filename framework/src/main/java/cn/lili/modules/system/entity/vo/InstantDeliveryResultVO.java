package cn.lili.modules.system.entity.vo;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 达达返回值
 *
 * @author Chopper
 * @date 2020/9/12 14:05
 */
@ApiModel
@Data
public class InstantDeliveryResultVO {
    @ApiModelProperty(value = "响应状态，成功为success，失败为fail", required = false)
    private String status;
    @ApiModelProperty(value = "响应返回码， 0 成功  其他失败", required = false)
    private Integer code;
    @ApiModelProperty(value = "响应描述", required = false)
    private String msg;
    @ApiModelProperty(value = "响应结果", required = false)
    private Object result;
}
