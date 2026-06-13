package cn.lili.common.security.sensitive;

import cn.lili.common.properties.SystemSettingProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.common.utils.SpringContextUtil;
import tools.jackson.core.JsonGenerator;
import tools.jackson.databind.BeanProperty;
import tools.jackson.databind.SerializationContext;
import tools.jackson.databind.ValueSerializer;
import tools.jackson.databind.ser.std.StdSerializer;

import java.util.Objects;

/**
 * 敏感信息序列化时 过滤
 *
 * @author liushuai(liushuai711 @ gmail.com)
 * @version v4.0
 * @Description:
 * @since 2021/9/10 16:46
 */
public class SensitiveJsonSerializer extends StdSerializer<String> {

    private SensitiveStrategy strategy;

    public SensitiveJsonSerializer() {
        super(String.class);
    }

    private SensitiveJsonSerializer(SensitiveStrategy strategy) {
        super(String.class);
        this.strategy = strategy;
    }

    @Override
    public void serialize(String value, JsonGenerator gen, SerializationContext serializers) {
        gen.writeString(strategy.desensitizer().apply(value));
    }

    @Override
    public ValueSerializer<?> createContextual(SerializationContext prov, BeanProperty property) {
        if (desensitization()) {
            Sensitive annotation = property.getAnnotation(Sensitive.class);
            if (Objects.nonNull(annotation) && Objects.equals(String.class, property.getType().getRawClass())) {
                return new SensitiveJsonSerializer(annotation.strategy());
            }
        }
        return prov.findValueSerializer(property.getType());
    }

    /**
     * 是否需要脱敏处理
     */
    private boolean desensitization() {
        AuthUser authUser = UserContext.getCurrentUser();
        if (authUser == null) {
            return false;
        }

        SystemSettingProperties systemSettingProperties = SpringContextUtil.getBean(SystemSettingProperties.class);

        if (authUser.getRole().equals(UserEnums.STORE)) {
            return systemSettingProperties.getSensitiveLevel() == 2;
        }

        if (authUser.getRole().equals(UserEnums.MANAGER)) {
            return systemSettingProperties.getSensitiveLevel() >= 1;
        }

        return false;
    }
}
