package cn.lili.buyer.test.bill;

import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.modules.store.entity.dto.StoreSettlementDay;
import cn.lili.modules.store.service.BillService;
import cn.lili.modules.store.service.StoreDetailService;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.List;

/**
 * @author paulG
 * @since 2022/1/10
 **/
@ExtendWith(SpringExtension.class)
@SpringBootTest
public class BillTest {


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

    @Test
    void createBillTest() {
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
        Assertions.assertTrue(true);
    }


}
