package cn.lili.modules.store.entity.enums;

/**
 * 店铺状态枚举
 *
 * @author pikachu
 * @since 2020年3月07日 上午11:04:25
 */
public enum StoreStatusEnum {
    /**
     * 开启中
     */
    OPEN("开启中"),
    /**
     * 店铺关闭
     */
    CLOSED("店铺关闭"),
    /**
     * 申请开店
     */
    APPLY("申请开店,只要完成第一步骤就是申请"),
    /**
     * 审核拒绝
     */
    REFUSED("审核拒绝"),
    /**
     * 申请中
     */
    APPLYING("申请中，提交审核");

    private final String description;

    StoreStatusEnum(String des) {
        this.description = des;
    }

    public String description() {
        return this.description;
    }

    public String value() {
        return this.name();
    }
}
