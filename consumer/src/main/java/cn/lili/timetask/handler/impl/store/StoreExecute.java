package cn.lili.timetask.handler.impl.store;

import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 店铺信息更新
 *
 * @author Chopper
 * @since 2021/3/15 5:30 下午
 */
@Component
@Slf4j
public class StoreExecute implements EveryDayExecute {
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public void execute() {
        //获取所有开启的店铺
        List<Store> storeList = storeService.list(new LambdaQueryWrapper<Store>().eq(Store::getStoreDisable, StoreStatusEnum.OPEN.name()));

        for (Store store : storeList) {
            try {
                Long num = goodsSkuService.countSkuNum(store.getId());
                storeService.updateStoreGoodsNum(store.getId(), num);
            } catch (Exception e) {
                log.error("店铺id为{},更新商品数量失败", store.getId(), e);
            }
        }


    }
}
