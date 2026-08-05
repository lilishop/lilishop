package cn.lili.timetask.handler.impl.distribution;

import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.timetask.handler.EveryDayExecute;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 分销客户关系到期清理
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@Slf4j
@Component
public class DistributionRelationExpireExecute implements EveryDayExecute {

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    /**
     * 每日定时将过期的短期客户关系置为失效。
     */
    @Override
    public void execute() {
        try {
            int count = customerRelationService.expirePendingRelations();
            if (count > 0) {
                log.info("分销客户关系到期处理完成，数量：{}", count);
            }
        } catch (Exception e) {
            log.error("分销客户关系到期处理异常：", e);
        }
    }
}
