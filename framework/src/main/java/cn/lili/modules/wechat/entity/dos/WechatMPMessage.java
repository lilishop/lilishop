package cn.lili.modules.wechat.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 微信小程序消息订阅
 * @author Chopper
 */
@Data
@TableName("li_wechat_mp_message")
@ApiModel(value = "微信小程序消息订阅")
public class WechatMPMessage extends BaseEntity {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "模版id")
    private String templateId;

    @ApiModelProperty(value = "模版名称")
    private String name;

    @ApiModelProperty(value = "微信模版码")
    private String code;

    @ApiModelProperty(value = "关键字")
    private String keywords;

    @ApiModelProperty(value = "关键字描述（小程序发送消息时使用）")
    private String keywordsText;

    @ApiModelProperty(value = "是否开启")
    private Boolean enable = true;

    @ApiModelProperty("订单状态")
    private String orderStatus;
}