package cn.lili.timetask.handler.impl.promotion;

import cn.lili.modules.promotion.service.GiftCardCashService;
import cn.lili.timetask.handler.EveryDayExecute;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 现金礼品卡过期扫描：将已到期的凭证状态批量更新为 EXPIRED。
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@Slf4j
@Component
public class GiftCardCashExpireExecute implements EveryDayExecute {

    @Autowired
    private GiftCardCashService giftCardCashService;

    @Override
    public void execute() {
        try {
            int count = giftCardCashService.expireOverdueCredentials();
            log.info("礼品卡过期定时任务执行完成，更新数量：{}", count);
        } catch (Exception e) {
            log.error("礼品卡过期定时任务异常", e);
        }
    }
}
