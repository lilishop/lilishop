package cn.lili.modules.system.entity.dto.payment;

import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.system.entity.dto.payment.dto.PaymentSupportForm;
import cn.lili.modules.system.entity.dto.payment.dto.PaymentSupportItem;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.ArrayList;
import java.util.List;

/**
 * 支持的支付方式
 *
 * @author Chopper
 * @since 2021-01-26 15:52
 */
@Data
@Accessors(chain = true)
public class PaymentSupportSetting {

    private List<PaymentSupportItem> paymentSupportItems;


    public PaymentSupportSetting() {

    }

    public PaymentSupportSetting(PaymentSupportForm paymentSupportForm) {

        List<PaymentSupportItem> paymentSupportItems = new ArrayList<>();

        for (ClientTypeEnum client : paymentSupportForm.getClients()) {
            PaymentSupportItem paymentSupportItem = new PaymentSupportItem();

            List<String> supports = new ArrayList<>();
            for (PaymentMethodEnum payment : paymentSupportForm.getPayments()) {
                supports.add(payment.name());
            }
            paymentSupportItem.setClient(client.name());
            paymentSupportItem.setSupports(supports);
            paymentSupportItems.add(paymentSupportItem);

        }
        this.paymentSupportItems = paymentSupportItems;
    }
}
