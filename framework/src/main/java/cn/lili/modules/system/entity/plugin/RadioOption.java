package cn.lili.modules.system.entity.plugin;

import lombok.Data;
import lombok.ToString;

/**
 * 单选配置类
 *
 * @author pikachu
 * @version v4.0
 * @Description:单选配置类
 * @since 2020/12/01 15:58
 */
@Data
@ToString
public class RadioOption {

    /**
     * 选项
     */
    private String label;

    /**
     * 选项值
     */
    private Object value;

}
