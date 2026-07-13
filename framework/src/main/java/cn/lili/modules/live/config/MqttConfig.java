package cn.lili.modules.live.config;

/**
 * MQTT消息工具配置
 *
 * @author chc
 * @since 2022/6/2114:46
 */
public class MqttConfig {
    public static final String BROKER_URL = "wss://lilishop-mqtt-push.dllll.xyz:443/mqtt"; // HiveMQ Broker URL
    public static final String CLIENT_ID_PUBLISHER = "java-publisher-" + System.currentTimeMillis();
    public static final String CLIENT_ID_SUBSCRIBER = "java-subscriber-" + System.currentTimeMillis();
    public static final String DEFAULT_TOPIC_PREFIX = "app/data/";

    public static final String LIVE = "live/";
}
