package cn.lili.common.security.sensitive;

import cn.lili.common.properties.SystemSettingProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.BeanProperty;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.fasterxml.jackson.databind.ser.ContextualSerializer;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;

import java.io.IOException;
import java.util.Objects;

/**
 * 敏感信息序列化时 过滤
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2021/9/10 16:46
 */
public class SensitiveJsonSerializer extends JsonSerializer<String>
        implements ContextualSerializer, ApplicationContextAware {
    private SensitiveStrategy strategy;

    //系统配置
    private SystemSettingProperties systemSettingProperties;

    @Override
    public void serialize(String value, JsonGenerator gen, SerializerProvider serializers) throws IOException {
        // 字段序列化处理
        gen.writeString(strategy.desensitizer().apply(value));
    }

    @Override
    public JsonSerializer<?> createContextual(SerializerProvider prov, BeanProperty property) throws JsonMappingException {

        // 判定是否 需要脱敏处理
        if (desensitization()) {
            //获取敏感枚举
            Sensitive annotation = property.getAnnotation(Sensitive.class);
            //如果有敏感注解，则加入脱敏规则
            if (Objects.nonNull(annotation) && Objects.equals(String.class, property.getType().getRawClass())) {
                this.strategy = annotation.strategy();
                return this;
            }
        }
        return prov.findValueSerializer(property.getType(), property);

    }

    @Override
    public void setApplicationContext(ApplicationContext applicationContext) throws BeansException {
        systemSettingProperties = applicationContext.getBean(SystemSettingProperties.class);
    }

    /**
     * 是否需要脱敏处理
     *
     * @return
     */
    private boolean desensitization() {

        //当前用户
        AuthUser authUser = UserContext.getCurrentUser();
        //默认脱敏
        if (authUser == null) {
            return false;
        }

        //如果是店铺
        if (authUser.getRole().equals(UserEnums.STORE)) {
            //店铺需要进行脱敏，则脱敏处理
            return systemSettingProperties.getSensitiveLevel() == 2;
        }


        //如果是店铺
        if (authUser.getRole().equals(UserEnums.MANAGER)) {
            //店铺需要进行脱敏，则脱敏处理
            return systemSettingProperties.getSensitiveLevel() >= 1;
        }

        return false;
    }
}
