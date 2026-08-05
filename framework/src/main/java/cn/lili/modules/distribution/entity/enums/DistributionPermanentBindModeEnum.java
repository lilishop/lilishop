package cn.lili.modules.distribution.entity.enums;

/**
 * 永久锁客生效方式
 *
 * @author pikachu
 */
public enum DistributionPermanentBindModeEnum {

    /** 扫码立即永久绑定 */
    SCAN_IMMEDIATE("扫码立即永久绑定"),

    /** 扫码后指定天数内支付成功转永久 */
    PAID_WITHIN_DAYS("支付后转永久");

    private final String description;

    DistributionPermanentBindModeEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}
