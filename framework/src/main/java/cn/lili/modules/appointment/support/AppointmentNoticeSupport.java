package cn.lili.modules.appointment.support;

import cn.lili.modules.message.entity.dto.NoticeMessageDTO;
import cn.lili.modules.message.entity.enums.NoticeMessageNodeEnum;
import cn.lili.modules.message.entity.enums.NoticeMessageParameterEnum;
import cn.lili.modules.message.service.NoticeMessageService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.HashMap;
import java.util.Map;

/**
 * 预约订单站内信通知
 *
 * @author Bulbasaur
 * @since 2026-09-18
 */
@Slf4j
@Component
public class AppointmentNoticeSupport {

    @Autowired
    private NoticeMessageService noticeMessageService;

    /**
     * 按节点发送预约站内信
     *
     * @param memberId  会员ID
     * @param node      消息节点
     * @param goodsName 商品名称
     * @param orderSn   订单编号
     */
    public void notifyMember(String memberId, NoticeMessageNodeEnum node, String goodsName, String orderSn) {
        send(memberId, node, goodsName, orderSn);
    }

    /**
     * 服务退款通知
     *
     * @param memberId  会员ID
     * @param goodsName 商品名称
     * @param orderSn   订单编号
     */
    public void notifyServiceRefunded(String memberId, String goodsName, String orderSn) {
        send(memberId, NoticeMessageNodeEnum.APPOINTMENT_SERVICE_REFUNDED_MEMBER, goodsName, orderSn);
    }

    /**
     * 发送预约站内信
     *
     * @param memberId  会员ID
     * @param node      消息节点
     * @param goodsName 商品名称
     * @param orderSn   订单编号
     */
    private void send(String memberId, NoticeMessageNodeEnum node, String goodsName, String orderSn) {
        if (memberId == null || node == null) {
            return;
        }
        try {
            NoticeMessageDTO dto = new NoticeMessageDTO();
            dto.setMemberId(memberId);
            dto.setNoticeMessageNodeEnum(node);
            Map<String, String> params = new HashMap<>(4);
            params.put(NoticeMessageParameterEnum.GOODS.getType(), goodsName == null ? "" : goodsName);
            params.put("orderSn", orderSn == null ? "" : orderSn);
            dto.setParameter(params);
            noticeMessageService.noticeMessage(dto);
        } catch (Exception e) {
            log.warn("预约站内信发送失败 node={}, orderSn={}", node, orderSn, e);
        }
    }
}
