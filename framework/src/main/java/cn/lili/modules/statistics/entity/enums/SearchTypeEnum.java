package cn.lili.modules.statistics.entity.enums;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;

/**
 * 搜索类型
 *
 * @author Chopper
 * @since 2021/2/9 16:17
 */
public enum SearchTypeEnum {

    /**
     * 昨天，今天，过去七天，过去30天
     */
    TODAY, YESTERDAY, LAST_SEVEN, LAST_THIRTY;

    /**
     * 快捷搜索类型
     *
     * @param searchType 快捷搜索类型
     * @return 快捷搜索类型枚举
     */
    public static SearchTypeEnum parse(String searchType) {
        if (searchType != null) {
            for (SearchTypeEnum searchTypeEnum : values()) {
                if (searchTypeEnum.name().equals(searchType)) {
                    return searchTypeEnum;
                }
            }
        }
        throw new ServiceException(ResultCode.PARAMS_ERROR);
    }
}
