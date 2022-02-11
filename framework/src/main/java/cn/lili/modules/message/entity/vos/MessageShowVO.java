package cn.lili.modules.message.entity.vos;

import cn.lili.modules.message.entity.enums.MessageShowType;
import cn.lili.modules.message.entity.enums.RangeEnum;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 消息
 *
 * @author Chopper
 * @since 2020/12/2 17:50
 */
@Data
@ApiModel(value = "消息")
@AllArgsConstructor
@NoArgsConstructor
public class MessageShowVO {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "标题")
    private String title;

    /**
     * @see MessageShowType
     */
    @ApiModelProperty(value = "消息类型")
    private String type;

    @ApiModelProperty(value = "消息内容")
    private String content;
    /**
     * @see RangeEnum
     */
    @ApiModelProperty(value = "发送范围")
    private String messageRange;

}