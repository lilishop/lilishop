package cn.lili.modules.system.entity.plugin;

import lombok.Data;
import lombok.ToString;

import java.util.List;

/**
 * 插件配置类
 *
 * @author pikachu
 * @version v4.0
 * @Description:
 * @since 2020/12/01 15:58
 */
@Data
@ToString
public class ConfigItem {
    /**
     * 配置文件name值
     */
    private String name;
    /**
     * 配置文件name映射文本值
     */
    private String text;
    /**
     * 配置文件显示在浏览器时，input的type属性
     */
    private String type;
    /**
     * 配置的值
     */
    private Object value;
    /**
     * 如果是select 是需要将可选项传递到前台
     */
    private List<RadioOption> options;


}
