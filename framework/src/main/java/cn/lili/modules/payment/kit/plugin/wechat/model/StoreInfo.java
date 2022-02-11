package cn.lili.modules.payment.kit.plugin.wechat.model;

import lombok.Data;
import lombok.experimental.Accessors;

/**
 * 统一下单-商户门店信息
 *
 * @author Chopper
 * @since 2020/12/17 17:58
 */

@Data
@Accessors(chain = true)
public class StoreInfo {
    /**
     * 门店编号
     */
    private String id;
    /**
     * 门店名称
     */
    private String name;
    /**
     * 地区编码
     */
    private String area_code;
    /**
     * 详细地址
     */
    private String address;
}
