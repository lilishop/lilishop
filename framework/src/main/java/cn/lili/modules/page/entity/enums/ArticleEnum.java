package cn.lili.modules.page.entity.enums;

/**
 * 文章分类枚举
 *
 * @author Bulbasaur
 * @since 2020/12/7 10:50
 */
public enum ArticleEnum {

    /**
     * 关于我们
     */
    ABOUT,
    /**
     * 隐私政策
     */
    PRIVACY_POLICY,
    /**
     * 用户协议
     */
    USER_AGREEMENT,
    /**
     * 证照信息
     */
    LICENSE_INFORMATION,
    /**
     * 店铺入驻
     */
    STORE_REGISTER,
    /**
     * 其他文章
     */
    OTHER;

    public String value() {
        return this.name();
    }

}
