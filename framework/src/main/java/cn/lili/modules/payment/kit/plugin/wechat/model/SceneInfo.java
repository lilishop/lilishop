package cn.lili.modules.payment.kit.plugin.wechat.model;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 统一下单-场景信息
 *
 * @author Chopper
 * @since 2020/12/17 17:57
 */

@Data
@Accessors(chain = true)
public class SceneInfo {
    /**
     * 用户终端IP
     */
    private String payer_client_ip;
    /**
     * 商户端设备号
     */
    private String device_id;
    /**
     * 商户门店信息
     */
    private StoreInfo store_info;
    /**
     * H5 场景信息
     */
    private H5Info h5_info;
}
