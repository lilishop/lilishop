package cn.lili.modules.system.entity.dto;

import lombok.Data;

import java.io.Serializable;

/**
 * 提现配置
 *
 * @author pikachu
 * @since 2020/11/30 15:23
 */
@Data
public class WithdrawalSetting implements Serializable {

    private static final long serialVersionUID = -3872782530832122976L;
    /**
     * 提现最低金额
     */
    private Double minPrice;
    /**
     * 提现手续费
     */
    private Double fee;
    /**
     * 提现类型 WECHAT\ALI
     */
    private String type;
    /**
     * 提现是否需要审核
     */
    private Boolean apply;

    /**
     * 微信提现-渠道使用的APPID
     */
    private String wechatAppId;
    /**
     * 微信APPID渠道
     */
    private String wechatAppIdSource;
}
