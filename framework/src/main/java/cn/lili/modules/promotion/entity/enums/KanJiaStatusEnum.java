package cn.lili.modules.promotion.entity.enums;

/**
 * 砍价活动状态状态枚举
 *
 * @author Chopper
 * @date 2020-03-19 3:53 下午
 */
public enum KanJiaStatusEnum {

    START("开始"), FAIL("失败"), SUCCESS("成功");

    private final String description;

    KanJiaStatusEnum(String str) {
        this.description = str;
    }

    public String description() {
        return description;
    }
}
