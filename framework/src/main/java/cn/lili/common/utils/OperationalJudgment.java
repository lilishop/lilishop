package cn.lili.common.utils;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;

/**
 * 全局统一判定是否可操作某属性
 *
 * @author Chopper
 * @version v1.0
 * 2020-08-20 18:07
 */
public class OperationalJudgment<t> {

    /**
     * 需要判定的对象必须包含属性 memberId，storeId 代表判定的角色
     *
     * @param object 判定的对象
     * @param <t>
     * @return
     */
    public static <t> t judgment(t object) {
        return judgment(object, "memberId", "storeId");
    }

    /**
     * 需要判定的对象必须包含属性 memberId，storeId 代表判定的角色
     *
     * @param object
     * @param buyerIdField
     * @param storeIdField
     * @param <t>
     * @return 返回判定本身，防止多次查询对象
     */
    public static <t> t judgment(t object, String buyerIdField, String storeIdField) {
        AuthUser tokenUser = UserContext.getCurrentUser();
        switch (tokenUser.getRole()) {
            case MANAGER:
                return object;
            case MEMBER:
                if (tokenUser.getId().equals(BeanUtil.getFieldValueByName(buyerIdField, object))) {
                    return object;
                } else {
                    throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
                }
            case STORE:
                if (tokenUser.getStoreId().equals(BeanUtil.getFieldValueByName(storeIdField, object))) {
                    return object;
                } else {
                    throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
                }
            default:
                throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        }
    }
}
