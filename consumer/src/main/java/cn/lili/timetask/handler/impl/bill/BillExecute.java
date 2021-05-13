package cn.lili.timetask.handler.impl.bill;

import cn.hutool.core.date.DateUtil;
import cn.lili.modules.store.entity.dto.StoreSettlementDay;
import cn.lili.modules.store.mapper.StoreDetailMapper;
import cn.lili.modules.store.service.BillService;
import cn.lili.timetask.handler.EveryDayExecute;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 店铺结算执行
 *
 * @author Bulbasaur
 * @date 2021/2/18 3:45 下午
 */
@Component
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class BillExecute implements EveryDayExecute {

    //结算单
    private final BillService billService;
    //店铺详情
    private final StoreDetailMapper storeDetailMapper;

    /**
     * 1.查询今日待结算的商家
     * 2.查询商家上次结算日期，生成本次结算单
     * 3.记录商家结算日
     */
    @Override
    public void execute() {

        //获取当前时间的前一天
        String day = "," + DateUtil.date().dayOfMonth() + ",";

        //获取待结算商家列表
        List<StoreSettlementDay> storeList = storeDetailMapper.getSettlementStore(new QueryWrapper<StoreSettlementDay>().like("settlement_cycle", day));

        //批量商家结算
        for (StoreSettlementDay storeSettlementDay : storeList) {

            //生成结算单
            billService.createBill(storeSettlementDay.getStoreId(), storeSettlementDay.getSettlementDay());

            //修改店铺结算时间
            storeDetailMapper.updateSettlementDay(storeSettlementDay.getStoreId(), DateUtil.date());
        }
    }
}
