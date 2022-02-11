package cn.lili.modules.goods.entity.enums;


/**
 * 直播间状态
 *
 * @author Bulbasaur
 * @since 2021/5/31 10:32 上午
 */
public enum StudioStatusEnum {

    /**
     * 新建
     */
    NEW("新建"),
    /**
     * 开始
     */
    START("开始"),
    /**
     * 结束
     */
    END("结束");

    private final String clientName;

    StudioStatusEnum(String des) {
        this.clientName = des;
    }

    public String clientName() {
        return this.clientName;
    }

}
