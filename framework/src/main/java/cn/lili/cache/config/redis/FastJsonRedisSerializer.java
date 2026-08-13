package cn.lili.cache.config.redis;

import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONReader;
import com.alibaba.fastjson2.JSONWriter;
import com.alibaba.fastjson2.filter.Filter;
import org.springframework.data.redis.serializer.RedisSerializer;

import java.nio.charset.StandardCharsets;

/**
 * 要实现对象的缓存，定义自己的序列化和反序列化器。使用阿里的fastjson来实现的比较多
 *
 * @author Bulbasaur
 */
public class FastJsonRedisSerializer<T> implements RedisSerializer<T> {
    static final Filter autoTypeFilter = JSONReader.autoTypeFilter(
            // 按需加上需要支持自动类型的类名前缀，范围越小越安全
            "cn.lili.",
            "cn.hutool.json.",
            "java.util."
    );

    private final Class<T> clazz;

    public FastJsonRedisSerializer(Class<T> clazz) {
        super();
        this.clazz = clazz;
    }

    @Override
    public byte[] serialize(T t) {
        if (t == null) {
            return new byte[0];
        }
        return JSON.toJSONString(t,
                        JSONWriter.Feature.WriteClassName)
                .getBytes(StandardCharsets.UTF_8);
    }

    @Override
    public T deserialize(byte[] bytes) {
        if (bytes == null || bytes.length == 0) {
            return null;
        }
        String str = new String(bytes, StandardCharsets.UTF_8);
        try {
            return parseRedisJson(str);
        } catch (Throwable ex) {
            // 出现解析异常时，尝试对 JSON 文本进行非法字符清洗后再次解析
            String cleaned = sanitizeRedisJson(str);
            if (!cleaned.equals(str)) {
                try {
                    return parseRedisJson(cleaned);
                } catch (Throwable ex2) {
                    throw ex2;
                }
            }
            throw ex;
        }
    }

    private T parseRedisJson(String str) {
        return JSON.parseObject(
                prepareRedisJson(str),
                clazz,
                autoTypeFilter,
                JSONReader.Feature.SupportAutoType
        );
    }

    /**
     * 规范化 fastjson2 WriteClassName 产生的空 HashMap，避免泛型 Map 反序列化失败。
     */
    static String prepareRedisJson(String str) {
        return normalizeEmptyTypedMaps(str);
    }

    private static String normalizeEmptyTypedMaps(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        // 仅匹配不含其它键的空 Map，避免误伤带数据的 HashMap
        return str.replaceAll("\\{\"@type\":\"java\\.util\\.HashMap\"\\}", "{}");
    }

    /**
     * 清理可能导致 fastjson2 解析失败的非法字符。
     * 修复嵌套 JSON/HTML 字段中出现的无效反引号和未转义的双引号。
     */
    private static String sanitizeRedisJson(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        String cleaned = str;

        // 针对常见的高风险字符串字段做定点清洗
        String[] riskyKeys = {"specs", "intro", "mobileIntro", "small", "thumbnail"};
        for (String key : riskyKeys) {
            String prefix = "\"" + key + "\":\"";
            int from = 0;
            while (true) {
                int keyIdx = cleaned.indexOf(prefix, from);
                if (keyIdx == -1) {
                    break;
                }
                int valueStart = keyIdx + prefix.length();

                int endQuote = findEndQuoteForSpecs(cleaned, valueStart);
                if (endQuote != -1) {
                    cleaned = sanitizeSubstring(cleaned, valueStart, endQuote);
                    from = endQuote;
                } else {
                    // 无法定位，移动游标避免死循环
                    from = valueStart;
                }
            }
        }

        // 全局修复：去除反引号
        cleaned = cleaned.replace("`", "");

        // 修复 JSON 中出现的数字后缀 L（如 0L、123L）为合法数字
        // 例如 "payPoint":0L 或 ": 123L," -> ": 123,"
        cleaned = cleaned.replaceAll(":\\s*(-?\\d+)L([,}\\s])", ":$1$2");

        return cleaned;
    }

    private static String sanitizeSubstring(String text, int start, int endQuoteIndex) {
        String value = text.substring(start, endQuoteIndex);
        // 先移除反引号，避免 \` 影响后续转义判断
        value = value.replace("`", "");

        StringBuilder sb = new StringBuilder(value.length() + 16);
        for (int i = 0; i < value.length(); i++) {
            char ch = value.charAt(i);
            if (ch == '"' && (i == 0 || value.charAt(i - 1) != '\\')) {
                // 为所有未转义的双引号补上反斜杠
                sb.append('\\').append('"');
            } else {
                sb.append(ch);
            }
        }
        String fixed = sb.toString();
        return text.substring(0, start) + fixed + text.substring(endQuoteIndex);
    }

    private static int findEndQuoteForSpecs(String s, int start) {
        for (int i = start; i < s.length(); i++) {
            char c = s.charAt(i);
            if (c == '"') {
                // 未被反斜杠转义的引号
                boolean escaped = (i > start && s.charAt(i - 1) == '\\');
                if (!escaped) {
                    int j = i + 1;
                    // 跳过空白
                    while (j < s.length()) {
                        char w = s.charAt(j);
                        if (w == ' ' || w == '\t' || w == '\n' || w == '\r') {
                            j++;
                        } else {
                            break;
                        }
                    }
                    if (j < s.length()) {
                        char next = s.charAt(j);
                        if (next == ',' || next == '}') {
                            return i;
                        }
                    }
                }
            }
        }
        return -1;
    }
}