package cn.lili.trigger.message;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 拼团订单延时任务信息
 *
 * @author paulG
 * @since 2021/5/7
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PintuanOrderMessage {

    /**
     * 拼团活动id
     */
    private String pintuanId;

    /**
     * 父拼团订单sn
     */
    private String orderSn;


}
