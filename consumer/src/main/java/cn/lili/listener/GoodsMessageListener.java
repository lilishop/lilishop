package cn.lili.listener;

import cn.lili.annotation.GoodsMQTagRoute;
import cn.lili.common.aop.annotation.RetryOperation;
import cn.lili.dispatcher.goods.GoodsMessageDispatcherCenter;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.common.message.MessageExt;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

/**
 * 商品消息
 *
 * @author paulG
 * @since 2020/12/9
 **/
@Component
@Slf4j
@RocketMQMessageListener(topic = "${lili.data.rocketmq.goods-topic}", consumerGroup = "${lili.data.rocketmq.goods-group}")
public class GoodsMessageListener implements RocketMQListener<MessageExt> {

    // 1. 注入我们的处理中心
    @Autowired
    private GoodsMessageDispatcherCenter dispatcherCenter;
    // 2. 内存路由表：Key 是 Tag 字符串，Value 是要执行的方法 (Method)
    private final Map<GoodsTagsEnum, Method> handlerMap = new HashMap<>();


    @Override
    @RetryOperation
    public void onMessage(MessageExt messageExt) {
        String tag = messageExt.getTags();
        // 4. 从路由表里直接拿方法
        Method method = handlerMap.get(tag);

        if (method != null) {
            try {
                // 👑 终极绝杀：利用反射，一枪爆头！
                method.invoke(dispatcherCenter, messageExt.getBody());
            } catch (Exception e) {
                log.error(" 路由反射执行异常, Tag: {}", tag, e);
            }
        } else {
            log.warn("⚠️ 未知的会员消息 Tag: {}，已被直接丢弃", tag);
        }
    }


    // 3. 🚀 Spring 启动时自动执行，扫描并组装路由表
    @PostConstruct
    public void init() {
        // 💡 防止被 Spring CGLIB 代理导致找不到注解！
        Class<?> targetClass = AopUtils.getTargetClass(dispatcherCenter);

        // 拿到这个类里的所有方法
        Method[] methods = targetClass.getDeclaredMethods();
        for (Method method : methods) {
            // 找找哪个方法头上戴了咱们的 @MqTagHandler 绿帽子
            GoodsMQTagRoute annotation = method.getAnnotation(GoodsMQTagRoute.class);
            if (annotation != null) {
                // 如果有，把它的 Tag 名字和方法本身，丢进路由表！
                handlerMap.put(annotation.value(), method);
                log.info("🎯 成功注册 MQ 路由: Tag [{}] -> 方法 [{}]", annotation.value(), method.getName());
            }
        }
    }
}
