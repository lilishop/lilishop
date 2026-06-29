package cn.lili.modules.live.util;

import cn.lili.modules.live.config.MqttConfig;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.eclipse.paho.client.mqttv3.*;
import org.eclipse.paho.client.mqttv3.persist.MemoryPersistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;


/**
 * MQTT消息工具类 (重构后版本)
 *
 * @author chc
 * @since 2022/6/21
 */
@Component
public class MqttUtil {

    // 使用 SLF4J Logger，这是Java社区的最佳实践
    private static final Logger logger = LoggerFactory.getLogger(MqttUtil.class);

    private IMqttClient publisherClient;
    private IMqttClient subscriberClient;

    /**
     * 构造函数保持简洁。
     * 只进行最基本的初始化，不执行任何可能失败的I/O操作。
     */
    public MqttUtil() {
        logger.info("MqttUtil bean is being constructed.");
    }

    /**
     * 使用 @PostConstruct 注解，在Bean初始化完成后执行MQTT连接。
     * 这是处理资源初始化的标准Spring方式。
     */
    @PostConstruct
    public void init() {
        connect();
    }

    private void connect() {
        try {
            // ================== 1. 配置和连接发布者客户端 ==================
            // 注意：将初始化和连接逻辑放在一起
            publisherClient = new MqttClient(MqttConfig.BROKER_URL, MqttConfig.CLIENT_ID_PUBLISHER, new MemoryPersistence());
            MqttConnectOptions connOptsPub = createConnectOptions(true); // 自动重连

            publisherClient.setCallback(new MqttCallback() {
                @Override
                public void connectionLost(Throwable cause) {
                    // 只记录日志，Paho会自动处理重连
                    logger.error("Publisher connection lost: {}", cause.getMessage());
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) {}

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {}
            });

            logger.info("Connecting publisher to broker: {}", MqttConfig.BROKER_URL);
            publisherClient.connect(connOptsPub);
            logger.info("Publisher successfully initiated connection process.");


            // ================== 2. 配置和连接订阅者客户端 ==================
            subscriberClient = new MqttClient(MqttConfig.BROKER_URL, MqttConfig.CLIENT_ID_SUBSCRIBER, new MemoryPersistence());
            MqttConnectOptions connOptsSub = createConnectOptions(true); // 自动重连

            subscriberClient.setCallback(new MqttCallbackExtended() {
                @Override
                public void connectComplete(boolean reconnect, String serverURI) {
                    logger.info("Subscriber connectComplete. ServerURI: {}, Is Reconnect: {}", serverURI, reconnect);
                    // 无论首次连接还是重连，都执行订阅操作，保证订阅的健壮性
                    subscribeToDefaultTopics();
                }

                @Override
                public void connectionLost(Throwable cause) {
                    logger.error("Subscriber connection lost: {}", cause.getMessage());
                    // 无需任何操作，Paho的自动重连机制正在后台工作
                }

                @Override
                public void messageArrived(String topic, MqttMessage message) throws Exception {
                    String payload = new String(message.getPayload());
                    logger.info("Message arrived on topic '{}': {}", topic, payload);
                    // 在这里处理接收到的消息
                }

                @Override
                public void deliveryComplete(IMqttDeliveryToken token) {}
            });

            logger.info("Connecting subscriber to broker: {}", MqttConfig.BROKER_URL);
            subscriberClient.connect(connOptsSub);
            logger.info("Subscriber successfully initiated connection process.");

        } catch (MqttException me) {
            // 在启动时连接失败，这是可预期的。
            // Paho的自动重连机制会接管，我们只需记录日志。
            logger.error("MQTT initial connection failed: {}. The client will attempt to reconnect automatically in the background.", me.getMessage());
        }
    }

    /**
     * 辅助方法，创建统一的连接选项
     */
    private MqttConnectOptions createConnectOptions(boolean autoReconnect) {
        MqttConnectOptions options = new MqttConnectOptions();
        options.setAutomaticReconnect(autoReconnect);
        options.setCleanSession(false); // 持久化会话，非常重要
        options.setConnectionTimeout(10); // 连接超时
        options.setKeepAliveInterval(60); // 心跳
        // 如果需要用户名密码认证
        // options.setUserName("your_username");
        // options.setPassword("your_password".toCharArray());
        return options;
    }


    private void subscribeToDefaultTopics() {
        try {
            // 这里不需要检查 isConnected()，因为这个方法只在 connectComplete 回调中被调用，此时连接必然是成功的。
            logger.info("Attempting to subscribe to topics...");
            subscriberClient.subscribe(MqttConfig.DEFAULT_TOPIC_PREFIX + "#", 1);
            logger.info("Successfully subscribed to topic: {}", MqttConfig.DEFAULT_TOPIC_PREFIX + "#");
        } catch (MqttException me) {
            logger.error("Error subscribing to topics: {}", me.getMessage(), me);
        }
    }

    public void publishMessage(String topic, String payload, int qos, boolean retained) {
        // 在发布前检查连接状态是很好的习惯
        if (publisherClient == null || !publisherClient.isConnected()) {
            logger.warn("Publisher client is not connected. Message to topic '{}' will be queued if QoS > 0 and cleanSession=false.", topic);
            // 注意：即使这里不在线，如果QoS>0且cleanSession=false, Paho会尝试缓存消息并在重连后发送。
            // 但如果立即就需要反馈，你可以在这里抛出异常。
        }
        try {
            MqttMessage message = new MqttMessage(payload.getBytes());
            message.setQos(qos);
            message.setRetained(retained);
            logger.info("publishing message to topic: {}", topic);
            publisherClient.publish(topic, message);
        } catch (MqttException me) {
            logger.error("Error publishing message to topic '{}': {}", topic, me.getMessage(), me);
        }
    }

    /**
     * 使用 @PreDestroy 注解，在应用关闭时优雅地断开连接。
     */
    @PreDestroy
    public void disconnect() {
        logger.info("Disconnecting MQTT clients as the application is shutting down.");
        try {
            if (publisherClient != null && publisherClient.isConnected()) {
                publisherClient.disconnect();
                publisherClient.close();
                logger.info("Publisher disconnected.");
            }
            if (subscriberClient != null && subscriberClient.isConnected()) {
                subscriberClient.disconnect();
                subscriberClient.close();
                logger.info("Subscriber disconnected.");
            }
        } catch (MqttException me) {
            logger.error("Error during MQTT client disconnection: {}", me.getMessage(), me);
        }
    }

    // 你的其他业务方法，如 sendSensorData
    public void sendSensorData(String sensorId, String data) {
        String topic = MqttConfig.LIVE + sensorId;
        publishMessage(topic, data, 1, false);
    }
}