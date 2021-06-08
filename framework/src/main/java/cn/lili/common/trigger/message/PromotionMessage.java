package cn.lili.common.trigger.message;

import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 信息队列传输促销信息实体
 *
 * @author paulG
 * @date 2020/10/30
 **/
@Data
@AllArgsConstructor
@NoArgsConstructor
public class PromotionMessage {

    /**
     * 促销id
     */
    private String promotionId;
    /**
     * 促销类型
     */
    private String promotionType;

    /**
     * 促销状态
     */
    private String promotionStatus;

    /**
     * 开始时间
     */
    private Date startTime;

    /**
     * 结束时间
     */
    private Date endTime;

    public <T> UpdateWrapper<T> updateWrapper() {
        UpdateWrapper<T> updateWrapper = new UpdateWrapper<>();
        updateWrapper.eq("id", promotionId);
        updateWrapper.set("promotion_status", PromotionStatusEnum.valueOf(promotionStatus));
        return updateWrapper;
    }
}
