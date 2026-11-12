package cn.lili.common.properties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * 消息队列自定义配置属性
 * <p>
 * 绑定配置前缀 {@code lili.data.rocketmq}，用于集中管理系统中各核心业务模块
 * （促销、订单、商品、会员、通知、店铺、售后、库存流水等）的消息主题（Topic）与消费者分组（Group）。
 * </p>
 *
 * @author paulG
 * @since 2020/10/30
 **/
@Component
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@ConfigurationProperties(prefix = "lili.data.rocketmq")
public class RocketmqCustomProperties {

    /**
     * 默认通用消费者分组
     * <p>
     * 用于简化配置：当具体业务分组未单独指定时，各分组 getter 可统一回退使用该默认分组，
     * 避免在配置文件中为每个业务重复定义多套 {@code xxx-group}。
     * </p>
     */
    @Builder.Default
    private String defaultGroup = "lili_consumer_group";

    /**
     * 促销活动相关主题（如秒杀、拼团、优惠券等定时/延时任务与状态变更）
     */
    private String promotionTopic;

    /**
     * 促销活动消费者分组
     */
    private String promotionGroup;

    /**
     * 订单交易相关主题（如创建订单、支付完成、状态流转、核销发货等）
     */
    private String orderTopic;

    /**
     * 订单交易消费者分组
     */
    private String orderGroup;

    /**
     * 商品变动相关主题（如商品增删改、上下架、类目/参数变动等）
     */
    private String goodsTopic;

    /**
     * 商品变动消费者分组
     */
    private String goodsGroup;

    /**
     * 会员用户相关主题（如注册、登录、签到、提现申请、评价等）
     */
    private String memberTopic;

    /**
     * 会员用户消费者分组
     */
    private String memberGroup;

    /**
     * 站内通知与公告相关主题（如站内信、全员公告发送）
     */
    private String noticeTopic;

    /**
     * 站内通知与公告消费者分组
     */
    private String noticeGroup;

    /**
     * 消息外发相关主题（如短信验证码、模板消息、三方推送等）
     */
    private String noticeSendTopic;

    /**
     * 消息外发消费者分组
     */
    private String noticeSendGroup;

    /**
     * 店铺/商家相关主题（如店铺入驻审核、店铺设置变更等）
     */
    private String storeTopic;

    /**
     * 店铺/商家消费者分组
     */
    private String storeGroup;

    /**
     * 售后退款退货相关主题（如售后单创建、审核、退款状态变更等）
     */
    private String afterSaleTopic;

    /**
     * 售后退款退货消费者分组
     */
    private String afterSaleGroup;

    /**
     * 库存流水相关主题（如出库、入库、盘点调库流水）
     */
    @Builder.Default
    private String stockFlowTopic = "lili_stock_flow_topic";

    /**
     * 获取促销活动消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getPromotionGroup() {
        return (promotionGroup != null && !promotionGroup.isBlank()) ? promotionGroup : defaultGroup;
    }

    /**
     * 获取订单交易消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getOrderGroup() {
        return (orderGroup != null && !orderGroup.isBlank()) ? orderGroup : defaultGroup;
    }

    /**
     * 获取商品变动消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getGoodsGroup() {
        return (goodsGroup != null && !goodsGroup.isBlank()) ? goodsGroup : defaultGroup;
    }

    /**
     * 获取会员用户消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getMemberGroup() {
        return (memberGroup != null && !memberGroup.isBlank()) ? memberGroup : defaultGroup;
    }

    /**
     * 获取站内通知消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getNoticeGroup() {
        return (noticeGroup != null && !noticeGroup.isBlank()) ? noticeGroup : defaultGroup;
    }

    /**
     * 获取消息外发消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getNoticeSendGroup() {
        return (noticeSendGroup != null && !noticeSendGroup.isBlank()) ? noticeSendGroup : defaultGroup;
    }

    /**
     * 获取店铺相关消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getStoreGroup() {
        return (storeGroup != null && !storeGroup.isBlank()) ? storeGroup : defaultGroup;
    }

    /**
     * 获取售后相关消费者分组（若未单独配置则回退到默认分组）
     *
     * @return 消费者分组名称
     */
    public String getAfterSaleGroup() {
        return (afterSaleGroup != null && !afterSaleGroup.isBlank()) ? afterSaleGroup : defaultGroup;
    }
}

