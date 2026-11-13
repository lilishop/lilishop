package cn.lili.modules.goods.support;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.mapper.GoodsMapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

/**
 * 按店铺维护商品上下架状态，供店铺关闭和商品管理共用，避免店铺回调完整商品服务。
 * 保留原有当前店铺权限条件、指定店铺条件、定时状态清理及SKU联动范围。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Component
public class GoodsStoreMarketSupport {
    private final GoodsMapper mapper;
    private final GoodsSkuStatusSupport skuStatus;

    /**
     * 注入商品存储和独立SKU状态同步组件。
     *
     * @param mapper 商品存储
     * @param skuStatus SKU状态与缓存维护
     */
    public GoodsStoreMarketSupport(GoodsMapper mapper, GoodsSkuStatusSupport skuStatus) {
        this.mapper = mapper;
        this.skuStatus = skuStatus;
    }

    /**
     * 店铺关闭时以原原因下架商品，商品与SKU写入在同一事务内完成，不改变店铺关闭入口事务。
     *
     * @param storeId 待关闭店铺主键
     */
    @Transactional(rollbackFor = Exception.class)
    public void underStoreGoods(String storeId) {
        updateByStoreId(storeId, GoodsStatusEnum.DOWN, "店铺关闭");
    }

    /**
     * 更新商品状态及原因并清空对应定时上下架字段，商品查询同时保留当前店铺和指定店铺条件。
     * SKU按指定店铺独立同步，保留原有执行顺序及缓存范围；本方法不额外开启商品更新事务，
     * 无外层事务时SKU组件自行开启事务，店铺关闭入口则将商品和SKU写入统一纳入事务。
     *
     * @param storeId 指定店铺主键
     * @param status 目标上下架状态
     * @param underReason 商品上下架原因
     * @return 商品更新影响行数大于零时为true
     */
    public Boolean updateByStoreId(String storeId, GoodsStatusEnum status, String underReason) {
        AuthUser authUser = checkStoreAuthority();
        LambdaUpdateWrapper<Goods> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(Goods::getStoreId, authUser.getStoreId());
        updateWrapper.set(Goods::getMarketEnable, status.name());
        updateWrapper.set(Goods::getUnderMessage, underReason);
        if (GoodsStatusEnum.UPPER.equals(status)) {
            updateWrapper.set(Goods::getScheduledUpperTime, null);
            updateWrapper.set(Goods::getScheduledUpperReason, null);
        } else if (GoodsStatusEnum.DOWN.equals(status)) {
            updateWrapper.set(Goods::getScheduledDownTime, null);
            updateWrapper.set(Goods::getScheduledDownReason, null);
        }
        updateWrapper.eq(Goods::getStoreId, storeId);
        boolean result = mapper.update(null, updateWrapper) > 0;
        skuStatus.updateByStoreId(storeId, status.name(), null);
        return result;
    }

    /**
     * 校验当前登录用户为店铺身份，沿用原有角色判定和异常，不额外更改店铺归属条件。
     *
     * @return 当前登录店铺用户
     * @throws ServiceException 未登录或不是店铺身份时抛出
     */
    public AuthUser checkStoreAuthority() {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser == null || currentUser.getRole() == null || !currentUser.getRole().equals(UserEnums.STORE)) {
            throw new ServiceException(ResultCode.STORE_NOT_LOGIN_ERROR);
        }
        return currentUser;
    }
}
