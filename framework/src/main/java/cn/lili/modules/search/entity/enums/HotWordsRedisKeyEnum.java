package cn.lili.modules.search.entity.enums;

/**
 * @author paulG
 * @date 2021/1/20
 **/
public enum HotWordsRedisKeyEnum {

    /**
     * "搜索热词"
     */
    SEARCH_HOT_WORD("搜索热词");


    private final String description;

    HotWordsRedisKeyEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }
}
