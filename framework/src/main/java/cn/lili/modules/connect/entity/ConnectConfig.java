package cn.lili.modules.connect.entity;

import cn.lili.base.BaseEntity;
import cn.lili.modules.connect.entity.enums.ConnectConfigEnum;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Entity;
import javax.persistence.Table;
import java.util.HashMap;
import java.util.Map;

/**
 * @author Chopper
 */
@Data
@Entity
@Table(name = "li_connect_config")
@TableName("li_connect_config")
@ApiModel(value = "联合登陆配置")
@NoArgsConstructor
public class ConnectConfig extends BaseEntity {

    private static final long serialVersionUID = 1L;

    /**
     * @see cn.lili.modules.connect.entity.enums.ConnectConfigEnum
     */
    @ApiModelProperty(value = "配置key")
    private String configKey;


    @ApiModelProperty(value = "配置")
    private String configValue;

    public ConnectConfig(String configKey) {
        this.configKey = configKey;
        ConnectConfigEnum configEnum = ConnectConfigEnum.valueOf(configKey);
        String[] formItems = configEnum.getForm().split(",");
        Map<String, String> config = new HashMap<>(formItems.length);
        for (int i = 0; i < formItems.length; i++) {
            config.put(formItems[i], "");
        }
        this.configValue = config.toString();
    }
}