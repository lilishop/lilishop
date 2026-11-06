package cn.lili.modules.promotion.serviceimpl;

import cn.lili.modules.order.order.entity.dos.OrderGiftCardRecord;
import cn.lili.modules.order.order.mapper.OrderGiftCardRecordMapper;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * 根据原始提货兑换流水查询订单来源，取消处理记录不替换或删除原关联。
 *
 * @author Bulbasaur
 * @since 2026-11-06
 */
@Service
public class PickupCardOrderServiceImpl implements PickupCardOrderService {

    @Autowired
    private OrderGiftCardRecordMapper recordMapper;

    /** 根据订单编号获取原兑换流水，不以金额或支付方式推断订单来源。 */
    @Override
    public String getCredentialId(String orderSn) {
        if (orderSn == null || orderSn.isBlank()) return null;
        return getCredentialIds(List.of(orderSn)).get(orderSn);
    }

    /** 按当前页订单编号一次查询原兑换流水，空集合不发起数据库请求。 */
    @Override
    public Map<String, String> getCredentialIds(Collection<String> orderSns) {
        if (orderSns == null || orderSns.isEmpty()) return Map.of();
        List<String> sns = orderSns.stream().filter(Objects::nonNull).filter(sn -> !sn.isBlank()).distinct().toList();
        if (sns.isEmpty()) return Map.of();
        List<OrderGiftCardRecord> records = recordMapper.selectList(new LambdaQueryWrapper<OrderGiftCardRecord>()
                .select(OrderGiftCardRecord::getOrderSn, OrderGiftCardRecord::getCredentialId)
                .eq(OrderGiftCardRecord::getBizType, "PICKUP_REDEEM")
                .eq(OrderGiftCardRecord::getDeleteFlag, false)
                .in(OrderGiftCardRecord::getOrderSn, sns));
        Map<String, String> result = new LinkedHashMap<>();
        for (OrderGiftCardRecord record : records) {
            if (record.getCredentialId() != null) result.putIfAbsent(record.getOrderSn(), record.getCredentialId());
        }
        return result;
    }
}
