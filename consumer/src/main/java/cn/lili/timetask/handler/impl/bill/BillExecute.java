package cn.lili.timetask.handler.impl.bill;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.store.entity.dto.StoreSettlementDay;
import cn.lili.modules.store.service.BillService;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.timetask.handler.EveryDayExecute;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 店铺结算执行
 *
 * @author Bulbasaur
 * @since 2021/2/18 3:45 下午
 */
@Component
public class BillExecute implements EveryDayExecute {

    /**
     * 结算单
     */
    @Autowired
    private BillService billService;
    /**
     * 店铺详情
     */
    @Autowired
    private StoreDetailService storeDetailService;

    /**
     * 1.查询今日待结算的商家
     * 2.查询商家上次结算日期，生成本次结算单
     * 3.记录商家结算日
     */
    @Override
    public void execute() {

        //获取当前天数
        int day = DateUtil.date().dayOfMonth();

        //获取待结算商家列表
        List<StoreSettlementDay> storeList = storeDetailService.getSettlementStore(day);

        //获取当前时间
        DateTime endTime = DateUtil.date();
        //批量商家结算
        for (StoreSettlementDay storeSettlementDay : storeList) {

            //生成结算单
            billService.createBill(storeSettlementDay.getStoreId(), storeSettlementDay.getSettlementDay(), endTime);

            //修改店铺结算时间
            storeDetailService.updateSettlementDay(storeSettlementDay.getStoreId(), endTime);
        }
    }
}
