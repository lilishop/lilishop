package cn.lili.timetask.handler.impl.store;

import cn.lili.common.enums.SwitchEnum;
import cn.lili.modules.member.entity.vo.StoreRatingVO;
import cn.lili.modules.member.service.MemberEvaluationService;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.entity.enums.StoreStatusEnum;
import cn.lili.modules.store.service.StoreService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 店铺评分
 *
 * @author Chopper
 * @since 2021/3/15 5:30 下午
 */
@Component
public class StoreRatingExecute implements EveryDayExecute {
    /**
     * 店铺
     */
    @Autowired
    private StoreService storeService;
    /**
     * 会员评价
     */
    @Autowired
    private MemberEvaluationService memberEvaluationService;


    @Override
    public void execute() {
        //获取所有开启的店铺
        List<Store> storeList = storeService.list(new LambdaQueryWrapper<Store>().eq(Store::getStoreDisable, StoreStatusEnum.OPEN.name()));
        for (Store store : storeList) {
            //店铺所有开启的评价
            StoreRatingVO storeRatingVO = memberEvaluationService.getStoreRatingVO(store.getId(), SwitchEnum.OPEN.name());

            if (storeRatingVO != null) {
                //保存评分
                LambdaUpdateWrapper<Store> lambdaUpdateWrapper = Wrappers.lambdaUpdate();
                lambdaUpdateWrapper.eq(Store::getId, store.getId());
                lambdaUpdateWrapper.set(Store::getDescriptionScore, storeRatingVO.getDescriptionScore());
                lambdaUpdateWrapper.set(Store::getDeliveryScore, storeRatingVO.getDeliveryScore());
                lambdaUpdateWrapper.set(Store::getServiceScore, storeRatingVO.getServiceScore());
                storeService.update(lambdaUpdateWrapper);
            }

        }


    }
}
