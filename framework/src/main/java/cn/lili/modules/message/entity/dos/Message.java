package cn.lili.modules.message.entity.dos;

import cn.lili.mybatis.BaseEntity;
import cn.lili.modules.message.entity.enums.RangeEnum;
import cn.lili.modules.message.entity.enums.MessageSendClient;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 管理段发送消息对象
 *
 * @author lili
 */

@Data
@TableName("li_message")
@ApiModel(value = "消息")
public class Message extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "标题")
    private String title;

    @ApiModelProperty(value = "内容")
    private String content;

    /**
     * @see RangeEnum
     */
    @ApiModelProperty(value = "发送范围")
    private String messageRange;

    /**
     * @see MessageSendClient
     */
    @ApiModelProperty(value = "发送客户端 商家或者会员")
    private String messageClient;

    @TableField(exist = false)
    @ApiModelProperty(value = "发送指定用户id")
    private String[] userIds;

    @TableField(exist = false)
    @ApiModelProperty(value = "发送指定用户名称")
    private String[] userNames;
}