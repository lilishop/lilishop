package cn.lili.buyer.test.cart;

import cn.lili.common.utils.DateUtil;
import cn.lili.timetask.handler.impl.statistics.OnlineMemberStatistics;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.util.Calendar;
import java.util.Random;

/**
 * 订单库存扣减
 */
@ExtendWith(SpringExtension.class)
@SpringBootTest
class OnlineTest {

    @Autowired
    private OnlineMemberStatistics onlineMemberStatistics;

    //订单支付，库存扣减单元测试
    @Test
    void everyHour() {
        onlineMemberStatistics.execute();
    }

    //订单支付，库存扣减单元测试
    @Test
    void customSetting() {
        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - 48 );
        //循环填充数据
        for (int i = 0; i < 48; i++) {
            calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) + 1);
            System.out.println(DateUtil.toString(calendar.getTime(),""));
            Random random = new Random();
            onlineMemberStatistics.execute(calendar.getTime(), random.nextInt(1000000));
        }
    }

    public static void main(String[] args) {

        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - 48 - 1);
        //循环填充数据
        for (int i = 0; i < 48; i++) {
            calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) + 1);
            System.out.println(calendar.getTime().getTime());
        }
    }


}
