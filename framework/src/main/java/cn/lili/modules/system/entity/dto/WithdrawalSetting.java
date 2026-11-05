package cn.lili.modules.system.entity.dto;

import lombok.Data;
import jakarta.validation.constraints.DecimalMin;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;

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
     * 提现最低金额，单位元，必须为非负数。
     */
    @NotNull(message = "最低提现金额不能为空")
    @DecimalMin(value = "0", message = "最低提现金额必须是非负有效数字")
    private Double minPrice;
    /**
     * 提现手续费，沿用现有费率口径；未配置时为空，配置值不得为负。
     */
    @DecimalMin(value = "0", message = "提现手续费必须是非负有效数字")
    private Double fee;
    /**
     * 提现类型 WECHAT\ALI
     */
    @Pattern(regexp = "WECHAT|ALI", message = "提现类型只能为微信或支付宝")
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

    /**
     * 提现规则说明
     */
    private String withdrawRule;
}
