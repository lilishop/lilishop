package cn.lili.event.impl;

import cn.lili.event.TradeEvent;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

/**
 * 订单创建时保存预约时段与补充信息，不扣库存。
 *
 * @author Bulbasaur
 * @since 2026-09-20
 */
@Slf4j
@Service
public class OrderCreateAppointmentExecute implements TradeEvent {

    @Autowired
    private AppointmentService appointmentService;

    @Override
    public void orderCreate(TradeDTO tradeDTO) {
        try {
            appointmentService.persistOrderExtFromTrade(tradeDTO);
        } catch (Exception e) {
            log.error("预约订单扩展写入失败，tradeSn={}", tradeDTO == null ? null : tradeDTO.getSn(), e);
            throw e;
        }
    }
}
