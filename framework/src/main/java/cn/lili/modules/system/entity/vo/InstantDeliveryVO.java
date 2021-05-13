package cn.lili.modules.system.entity.vo;


import cn.lili.modules.system.entity.dos.InstantDelivery;
import cn.lili.modules.system.entity.plugin.ConfigItem;
import cn.lili.modules.system.entity.plugin.InstantDelivery.InstantDeliveryPlugin;
import com.google.common.reflect.TypeToken;
import com.google.gson.Gson;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

/**
 * 即时配送VO
 *
 * @author pikachu
 * @since 2020/12/01 15:58
 */
@Data
@ApiModel(value = "即时配送VO")
@AllArgsConstructor
@NoArgsConstructor
public class InstantDeliveryVO extends InstantDelivery {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "即时配送配置项", required = true)
    private List<ConfigItem> configItems;
    /**
     * 构建新的vo参数
     *
     * @param instantDelivery
     */
    public InstantDeliveryVO(InstantDelivery instantDelivery) {
        this.setCreateTime(instantDelivery.getCreateTime());
        this.setDeleteFlag(instantDelivery.getDeleteFlag());
        this.setId(instantDelivery.getId());
        this.setDeliveryName(instantDelivery.getDeliveryName());
        this.setDeliveryOpen(instantDelivery.getDeliveryOpen());
        this.setDeliveryBean(instantDelivery.getDeliveryBean());
        this.setImages(instantDelivery.getImages());
        Gson gson = new Gson();
        this.setConfigItems(gson.fromJson(instantDelivery.getDeliveryConfig(), new TypeToken<List<ConfigItem>>() {
        }.getType()));
    }

    /**
     * 根据插件构建默认参数
     *
     * @param instantDeliveryPlugin
     */
    public InstantDeliveryVO(InstantDeliveryPlugin instantDeliveryPlugin) {
        this.setId("0");
        this.setDeliveryName(instantDeliveryPlugin.getPluginName());
        this.setDeliveryOpen(instantDeliveryPlugin.getOpen());
        this.setDeliveryBean(instantDeliveryPlugin.getPluginId());
        this.setConfigItems(instantDeliveryPlugin.getDefaultConfigItem());
    }

}