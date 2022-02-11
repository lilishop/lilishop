package cn.lili.modules.page.entity.enums;

/**
 * 楼层装修枚举
 *
 * @author Bulbasaur
 * @since 2020/12/7 10:50
 */
public enum PageEnum {

    /**
     * 首页
     */
    INDEX,

    /**
     * 店铺
     */
    STORE,

    /**
     * 专题页面
     */
    SPECIAL;

    public String value() {
        return this.name();
    }

}
