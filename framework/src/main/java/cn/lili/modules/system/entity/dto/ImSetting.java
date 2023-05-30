package cn.lili.modules.system.entity.dto;

import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;

/**
 * IM设置
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:10 下午
 */
@Data
public class ImSetting implements Serializable {


    @ApiModelProperty(value = "平台地址")
    private String httpUrl;


}
