package cn.lili.modules.message.entity.dos;

import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;


/**
 * 短链接/暂时只用于小程序二维码业务
 * @author Chopper
 */
@Data
@TableName("li_short_link")
@ApiModel(value = "短链接/暂时只用于小程序二维码业务")
public class ShortLink extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "原始参数")
    private String originalParams;


}