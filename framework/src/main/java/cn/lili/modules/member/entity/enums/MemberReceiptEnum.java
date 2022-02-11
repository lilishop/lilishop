package cn.lili.modules.member.entity.enums;

/**
 * 发票类型
 *
 * @author Chopper
 * @since 2021-03-29 14:10:16
 */
public enum MemberReceiptEnum {

    /**
     * 发票类型
     */
    ELECTRONIC_INVOICE("电子发票"),
    ORDINARY_INVOICE("普通发票");

    private String description;

    MemberReceiptEnum(String str) {
        this.description = str;

    }

    public String description() {
        return description;
    }

}
