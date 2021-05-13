package cn.lili.common.trigger.model;

/**
 * @author paulG
 * @since 2020/8/20
 **/
public abstract class TimeExecuteConstant {

    /**
     * 促销延迟加载执行器
     */
    public static final String PROMOTION_EXECUTOR = "promotionTimeTriggerExecutor";

    /**
     * 拼团延迟加载执行器
     */
    public static final String PINTUAN_EXECUTOR = "pintuanTimeTriggerExecutor";
    
    /**
     * 拼团延迟加载执行器
     */
    public static final String FULL_DISCOUNT_EXECUTOR = "fullDiscountTimeTriggerExecutor";

}
