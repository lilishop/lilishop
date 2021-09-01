package cn.lili.modules.system.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 设置
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@Data
@TableName("li_setting")
@ApiModel(value = "配置")
@NoArgsConstructor
public class Setting extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "配置值value")
    private String settingValue;

}