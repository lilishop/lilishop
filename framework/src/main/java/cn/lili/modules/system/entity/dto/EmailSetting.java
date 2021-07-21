package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * 邮箱设置
 *
 * @author Chopper
 * @since 2020/11/26 15:58
 */
@Data
public class EmailSetting implements Serializable {

    private static final long serialVersionUID = 7261037221941716140L;
    @ApiModelProperty(value = "邮箱服务器")
    private String host;

    @ApiModelProperty(value = "发送者邮箱账号")
    private String username;

    @ApiModelProperty(value = "邮箱授权码")
    private String password;
}
