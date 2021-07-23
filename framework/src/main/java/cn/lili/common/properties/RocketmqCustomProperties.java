package cn.lili.common.properties;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
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

    private String promotionTopic;

    private String promotionGroup;

    private String orderTopic;

    private String orderGroup;

    private String msgExtTopic;

    private String msgExtGroup;

    private String goodsTopic;

    private String goodsGroup;

    private String topicUser;

    private String memberTopic;

    private String memberGroup;

    private String otherTopic;

    private String otherGroup;

    private String noticeTopic;

    private String noticeGroup;

    private String noticeSendTopic;

    private String noticeSendGroup;

    private String storeTopic;

    private String storeGroup;

    private String afterSaleTopic;

    private String afterSaleGroup;

    private String broadcastTopic;

    private String broadcastGroup;

}
