package cn.lili.modules.message.entity.vos;

import cn.lili.modules.message.entity.enums.MessageStatusEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 店铺消息查询
 *
 * @author Chopper
 * @since 2020/12/2 17:50
 */
@Data
@ApiModel(value = "消息")
public class StoreMessageQueryVO {

    private static final long serialVersionUID = 1L;

    /**
     * @see MessageStatusEnum
     */
    @ApiModelProperty(value = "状态")
    private String status;

    @ApiModelProperty(value = "消息id")
    private String messageId;

    @ApiModelProperty(value = "商家id")
    private String storeId;

}