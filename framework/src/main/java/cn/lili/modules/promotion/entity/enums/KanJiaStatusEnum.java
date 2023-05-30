package cn.lili.modules.promotion.entity.enums;

/**
 * 砍价活动状态状态枚举
 *
 * @author Chopper
 * @date 2020-03-19 3:53 下午
 */
public enum KanJiaStatusEnum {

    /**
     * 已开始
     */
    START("开始"),
    /**
     * 砍价失败
     */
    FAIL("失败"),
    /**
     * 砍价成功
     */
    SUCCESS("成功"),
    /**
     * 砍价活动结束
     * 已购买、超时未购买都是这个状态
     */
    END("已结束");

    private final String description;

    KanJiaStatusEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
