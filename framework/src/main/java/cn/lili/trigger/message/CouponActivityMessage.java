package cn.lili.trigger.message;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 信息队列传输促销信息实体
 *
 * @author paulG
 * @date 2020/10/30
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class CouponActivityMessage {

    /**
     * 促销id
     */
    private String couponActivityId;

}
