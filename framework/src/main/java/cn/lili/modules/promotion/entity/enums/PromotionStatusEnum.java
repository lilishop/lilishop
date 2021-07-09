package cn.lili.modules.promotion.entity.enums;

/**
 * 促销状态枚举
 *
 * @author Chopper
 * @date 2020-03-19 3:53 下午
 */
public enum PromotionStatusEnum {

    /**
     * 新建
     */
    NEW("新建"),
    /**
     * 开始/上架
     */
    START("开始/上架"),
    /**
     * 结束/下架
     */
    END("结束/下架"),
    /**
     * 紧急关闭/作废
     */
    CLOSE("紧急关闭/作废");

    private final String description;

    PromotionStatusEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
