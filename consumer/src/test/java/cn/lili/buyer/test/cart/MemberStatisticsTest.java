package cn.lili.buyer.test.cart;

import cn.lili.timetask.handler.impl.statistics.MemberStatisticsExecute;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;


@RunWith(SpringRunner.class)
@SpringBootTest
class MemberStatisticsTest {

    @Autowired
    private MemberStatisticsExecute memberStatisticsExecute;

    @Test
    void customSetting() {
        memberStatisticsExecute.execute();
    }
}
