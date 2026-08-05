package cn.lili.modules.distribution.entity.enums;

/**
 * 客户关系变更类型
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public enum DistributionCustomerRelationChangeTypeEnum {

    /** 建立绑定 */
    CREATE("CREATE", "绑定"),

    /** 关系过期 */
    EXPIRE("EXPIRE", "过期"),

    /** 后台换绑 */
    ADMIN_REBIND("ADMIN_REBIND", "换绑"),

    /** 后台解绑 */
    ADMIN_UNBIND("ADMIN_UNBIND", "解绑");

    private final String code;
    private final String label;

    DistributionCustomerRelationChangeTypeEnum(String code, String label) {
        this.code = code;
        this.label = label;
    }

    public String getCode() {
        return code;
    }

    public String getLabel() {
        return label;
    }

    public static String labelOf(String code) {
        if (code == null) {
            return "-";
        }
        for (DistributionCustomerRelationChangeTypeEnum value : values()) {
            if (value.code.equals(code)) {
                return value.label;
            }
        }
        return code;
    }
}
