package cn.lili.buyer.test.cart;

import cn.lili.timetask.handler.impl.statistics.MemberStatisticsExecute;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;


@ExtendWith(SpringExtension.class)
@SpringBootTest
class MemberStatisticsTest {

    @Autowired
    private MemberStatisticsExecute memberStatisticsExecute;

    @Test
    void customSetting() {
        memberStatisticsExecute.execute();
    }
}
