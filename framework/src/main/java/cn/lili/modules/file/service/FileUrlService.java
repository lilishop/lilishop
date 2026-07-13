package cn.lili.modules.file.service;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.properties.LocalFileProperties;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.OssSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.net.URI;
import java.time.temporal.Temporal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * 文件访问地址处理：保留数据库中的原始存储地址，在接口输出时按配置替换为 CDN 地址。
 */
@Service
public class FileUrlService {

    private static final String DEFAULT_LOCAL_PREFIX = "/files";

    private static final Pattern IMG_SRC_PATTERN = Pattern.compile("(?i)(<img\\b[^>]*?\\s(?:src|data-src)=)([\"'])(.*?)(\\2)");

    @Autowired(required = false)
    private SettingService settingService;

    @Autowired(required = false)
    private LocalFileProperties localFileProperties;

    public String toPublicUrl(String url) {
        return toPublicUrl(url, getOssSetting());
    }

    public String toPublicUrl(String url, OssSetting setting) {
        if (CharSequenceUtil.isBlank(url) || !cdnEnabled(setting)) {
            return url;
        }
        String normalizedCdn = normalizeCdnPrefix(setting.getCdnUrlPrefix());
        String trimmedUrl = url.trim();
        if (skipUrl(trimmedUrl)) {
            return url;
        }

        String localRequestPrefix = getLocalRequestPrefix(setting);
        String pathRewritten = rewriteLocalPath(trimmedUrl, localRequestPrefix, normalizedCdn);
        if (!CharSequenceUtil.equals(pathRewritten, trimmedUrl)) {
            return preserveOuterWhitespace(url, pathRewritten);
        }

        for (String sourcePrefix : getSourcePrefixes(setting)) {
            String rewritten = rewriteBySourcePrefix(trimmedUrl, sourcePrefix, normalizedCdn);
            if (!CharSequenceUtil.equals(rewritten, trimmedUrl)) {
                return preserveOuterWhitespace(url, rewritten);
            }
        }

        String absoluteLocal = rewriteAbsoluteLocalPath(trimmedUrl, localRequestPrefix, normalizedCdn);
        if (!CharSequenceUtil.equals(absoluteLocal, trimmedUrl)) {
            return preserveOuterWhitespace(url, absoluteLocal);
        }
        return url;
    }

    public String toImageUrl(String url, Integer width, Integer height) {
        return toImageUrl(url, width, height, getOssSetting());
    }

    public String toImageUrl(String url, Integer width, Integer height, OssSetting setting) {
        if (CharSequenceUtil.isBlank(url)) {
            return url;
        }
        String imageUrl = url;
        if (width != null && height != null && width > 0 && height > 0) {
            OssEnum ossEnum = getOssEnum(setting);
            switch (ossEnum) {
                case ALI_OSS:
                    imageUrl = appendQuery(url, "x-oss-process=style/" + width + "X" + height);
                    break;
                case HUAWEI_OBS:
                    imageUrl = appendQuery(url, "image/resize,m_fixed,h_" + height + ",w_" + width);
                    break;
                case TENCENT_COS:
                    imageUrl = appendQuery(url, "imageMogr2/thumbnail/" + width + "x" + height);
                    break;
                case MINIO:
                case LOCAL:
                default:
                    break;
            }
        }
        return toPublicUrl(imageUrl, setting);
    }

    public String rewriteText(String text) {
        return rewriteText(text, getOssSetting());
    }

    public String rewriteText(String text, OssSetting setting) {
        if (CharSequenceUtil.isBlank(text) || !cdnEnabled(setting) || !containsStorageCandidate(text, setting)) {
            return text;
        }
        String result = text;
        String trimmed = text.trim();
        if (trimmed.startsWith("{") || trimmed.startsWith("[")) {
            String rewrittenJson = rewriteJsonText(text, setting);
            if (!CharSequenceUtil.equals(rewrittenJson, text)) {
                return rewrittenJson;
            }
        }
        if (CharSequenceUtil.containsIgnoreCase(text, "<img")) {
            result = rewriteHtmlImages(result, setting);
        }
        if (result.contains(",")) {
            result = rewriteCommaSeparatedUrls(result, setting);
        }
        return toPublicUrl(result, setting);
    }

    public Object rewriteResponse(Object body) {
        return rewriteResponse(body, getOssSetting());
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    public Object rewriteResponse(Object body, OssSetting setting) {
        if (body == null || !cdnEnabled(setting)) {
            return body;
        }
        Set<Object> visited = Collections.newSetFromMap(new IdentityHashMap<>());
        if (body instanceof ResultMessage resultMessage) {
            resultMessage.setResult(rewriteValue(resultMessage.getResult(), setting, visited));
            return body;
        }
        return rewriteValue(body, setting, visited);
    }

    public OssSetting getOssSetting() {
        OssSetting setting = null;
        if (settingService != null) {
            try {
                Setting dbSetting = settingService.get(SettingEnum.OSS_SETTING.name());
                if (dbSetting != null && CharSequenceUtil.isNotBlank(dbSetting.getSettingValue())) {
                    setting = JSON.parseObject(dbSetting.getSettingValue(), OssSetting.class);
                }
            } catch (Exception ignored) {
                setting = null;
            }
        }
        if (setting == null) {
            setting = new OssSetting();
            setting.setType(OssEnum.LOCAL.name());
        }
        fillLocalDefault(setting);
        return setting;
    }

    private Object rewriteValue(Object value, OssSetting setting, Set<Object> visited) {
        if (value == null) {
            return null;
        }
        if (value instanceof String text) {
            return rewriteText(text, setting);
        }
        Class<?> valueClass = value.getClass();
        if (isSimpleType(valueClass)) {
            return value;
        }
        if (visited.contains(value)) {
            return value;
        }
        visited.add(value);

        if (value instanceof Map<?, ?> map) {
            rewriteMap(map, setting, visited);
            return value;
        }
        if (value instanceof List<?> list) {
            rewriteList(list, setting, visited);
            return value;
        }
        if (value instanceof Collection<?> collection) {
            collection.forEach(item -> rewriteValue(item, setting, visited));
            return value;
        }
        if (valueClass.isArray()) {
            rewriteArray(value, setting, visited);
            return value;
        }
        if (!shouldReflect(valueClass)) {
            return value;
        }
        rewriteFields(value, setting, visited);
        return value;
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void rewriteMap(Map<?, ?> map, OssSetting setting, Set<Object> visited) {
        for (Map.Entry entry : map.entrySet()) {
            Object rewritten = rewriteValue(entry.getValue(), setting, visited);
            try {
                entry.setValue(rewritten);
            } catch (UnsupportedOperationException ignored) {
                // 只读 Map 无法回写时跳过，避免影响接口主流程。
            }
        }
    }

    @SuppressWarnings({"unchecked", "rawtypes"})
    private void rewriteList(List<?> list, OssSetting setting, Set<Object> visited) {
        List rawList = list;
        for (int i = 0; i < rawList.size(); i++) {
            Object rewritten = rewriteValue(rawList.get(i), setting, visited);
            try {
                rawList.set(i, rewritten);
            } catch (UnsupportedOperationException ignored) {
                rewriteValue(rawList.get(i), setting, visited);
            }
        }
    }

    private void rewriteArray(Object array, OssSetting setting, Set<Object> visited) {
        int length = Array.getLength(array);
        for (int i = 0; i < length; i++) {
            Object rewritten = rewriteValue(Array.get(array, i), setting, visited);
            try {
                Array.set(array, i, rewritten);
            } catch (IllegalArgumentException ignored) {
                // 基础类型数组不需要替换。
            }
        }
    }

    private void rewriteFields(Object value, OssSetting setting, Set<Object> visited) {
        Class<?> type = value.getClass();
        while (type != null && type != Object.class) {
            for (Field field : type.getDeclaredFields()) {
                int modifiers = field.getModifiers();
                if (Modifier.isStatic(modifiers) || Modifier.isFinal(modifiers)) {
                    continue;
                }
                try {
                    field.setAccessible(true);
                    Object fieldValue = field.get(value);
                    Object rewritten = rewriteValue(fieldValue, setting, visited);
                    if (fieldValue != rewritten) {
                        field.set(value, rewritten);
                    }
                } catch (Exception ignored) {
                    // 某些第三方对象字段不可访问，跳过即可。
                }
            }
            type = type.getSuperclass();
        }
    }

    private String rewriteHtmlImages(String html, OssSetting setting) {
        Matcher matcher = IMG_SRC_PATTERN.matcher(html);
        StringBuffer buffer = new StringBuffer();
        while (matcher.find()) {
            String src = matcher.group(3);
            String rewritten = toPublicUrl(src, setting);
            matcher.appendReplacement(buffer, Matcher.quoteReplacement(matcher.group(1) + matcher.group(2) + rewritten + matcher.group(4)));
        }
        matcher.appendTail(buffer);
        return buffer.toString();
    }

    private String rewriteJsonText(String text, OssSetting setting) {
        try {
            Object json = JSON.parse(text);
            Set<Object> visited = Collections.newSetFromMap(new IdentityHashMap<>());
            Object rewritten = rewriteValue(json, setting, visited);
            return JSON.toJSONString(rewritten);
        } catch (JSONException ignored) {
            return text;
        }
    }

    private String rewriteCommaSeparatedUrls(String text, OssSetting setting) {
        String[] values = text.split(",", -1);
        boolean changed = false;
        for (int i = 0; i < values.length; i++) {
            String rewritten = toPublicUrl(values[i], setting);
            if (!CharSequenceUtil.equals(values[i], rewritten)) {
                values[i] = rewritten;
                changed = true;
            }
        }
        return changed ? String.join(",", values) : text;
    }

    private boolean containsStorageCandidate(String text, OssSetting setting) {
        if (CharSequenceUtil.isBlank(text)) {
            return false;
        }
        String localPrefix = getLocalRequestPrefix(setting);
        if (CharSequenceUtil.isNotBlank(localPrefix) && text.contains(CharSequenceUtil.addPrefixIfNot(localPrefix, "/"))) {
            return true;
        }
        for (String sourcePrefix : getSourcePrefixes(setting)) {
            if (CharSequenceUtil.isNotBlank(sourcePrefix) && text.contains(CharSequenceUtil.removeSuffix(sourcePrefix, "/"))) {
                return true;
            }
        }
        return false;
    }

    private String rewriteBySourcePrefix(String url, String sourcePrefix, String cdnPrefix) {
        if (CharSequenceUtil.isBlank(sourcePrefix)) {
            return url;
        }
        String normalizedSource = CharSequenceUtil.removeSuffix(sourcePrefix, "/");
        if (url.equals(normalizedSource)) {
            return cdnPrefix;
        }
        if (url.startsWith(normalizedSource + "/")) {
            return cdnPrefix + url.substring(normalizedSource.length());
        }
        return url;
    }

    private String rewriteLocalPath(String url, String localPrefix, String cdnPrefix) {
        if (CharSequenceUtil.isBlank(localPrefix)) {
            return url;
        }
        String normalizedLocal = normalizeRequestPath(localPrefix);
        if (url.equals(normalizedLocal)) {
            return cdnPrefix + normalizedLocal;
        }
        if (url.startsWith(normalizedLocal + "/") || url.startsWith(normalizedLocal + "?")) {
            return cdnPrefix + url;
        }
        return url;
    }

    private String rewriteAbsoluteLocalPath(String url, String localPrefix, String cdnPrefix) {
        try {
            URI uri = URI.create(url);
            if (!uri.isAbsolute()) {
                return url;
            }
            String path = CharSequenceUtil.blankToDefault(uri.getPath(), "");
            String normalizedLocal = normalizeRequestPath(localPrefix);
            if (!path.equals(normalizedLocal) && !path.startsWith(normalizedLocal + "/")) {
                return url;
            }
            String query = uri.getRawQuery() == null ? "" : "?" + uri.getRawQuery();
            String fragment = uri.getRawFragment() == null ? "" : "#" + uri.getRawFragment();
            return cdnPrefix + path + query + fragment;
        } catch (Exception ignored) {
            return url;
        }
    }

    private List<String> getSourcePrefixes(OssSetting setting) {
        List<String> prefixes = new ArrayList<>();
        switch (getOssEnum(setting)) {
            case ALI_OSS:
                addPrefix(prefixes, "https://" + setting.getAliyunOSSBucketName() + "." + setting.getAliyunOSSEndPoint());
                break;
            case MINIO:
                addPrefix(prefixes, setting.getM_frontUrl());
                break;
            case HUAWEI_OBS:
                addPrefix(prefixes, "https://" + setting.getHuaweicloudOBSBucketName() + "." + setting.getHuaweicloudOBSEndPoint());
                break;
            case TENCENT_COS:
                addPrefix(prefixes, "https://" + setting.getTencentCOSBucket() + ".cos." + setting.getTencentCOSRegion() + ".myqcloud.com");
                addPrefix(prefixes, setting.getTencentCOSEndPoint());
                break;
            case LOCAL:
                addPrefix(prefixes, setting.getLocalFileUrlPrefix());
                break;
            default:
                break;
        }
        return prefixes;
    }

    private void addPrefix(List<String> prefixes, String prefix) {
        if (CharSequenceUtil.isBlank(prefix)) {
            return;
        }
        try {
            URI uri = URI.create(prefix);
            if (uri.isAbsolute()) {
                prefixes.add(CharSequenceUtil.removeSuffix(prefix, "/"));
            }
        } catch (Exception ignored) {
            // 非完整域名的本地前缀由路径规则处理。
        }
    }

    private String appendQuery(String url, String query) {
        return url + (url.contains("?") ? "&" : "?") + query;
    }

    private boolean cdnEnabled(OssSetting setting) {
        return setting != null && Boolean.TRUE.equals(setting.getCdnEnabled()) && CharSequenceUtil.isNotBlank(setting.getCdnUrlPrefix());
    }

    private String normalizeCdnPrefix(String cdnPrefix) {
        return CharSequenceUtil.removeSuffix(CharSequenceUtil.nullToEmpty(cdnPrefix).trim(), "/");
    }

    private String getLocalRequestPrefix(OssSetting setting) {
        String prefix = setting == null ? null : setting.getLocalFileUrlPrefix();
        if (CharSequenceUtil.isBlank(prefix) && localFileProperties != null) {
            prefix = localFileProperties.getUrlPrefix();
        }
        if (CharSequenceUtil.isBlank(prefix)) {
            prefix = DEFAULT_LOCAL_PREFIX;
        }
        try {
            URI uri = URI.create(prefix);
            if (uri.isAbsolute() && CharSequenceUtil.isNotBlank(uri.getPath())) {
                return normalizeRequestPath(uri.getPath());
            }
        } catch (Exception ignored) {
            // 非 URI 配置按普通访问路径处理。
        }
        return normalizeRequestPath(prefix);
    }

    private String normalizeRequestPath(String path) {
        if (CharSequenceUtil.isBlank(path)) {
            return DEFAULT_LOCAL_PREFIX;
        }
        return CharSequenceUtil.addPrefixIfNot(CharSequenceUtil.removeSuffix(path.trim(), "/"), "/");
    }

    private OssEnum getOssEnum(OssSetting setting) {
        try {
            return OssEnum.valueOf(setting.getType());
        } catch (Exception ignored) {
            return OssEnum.LOCAL;
        }
    }

    private boolean skipUrl(String url) {
        String lower = url.toLowerCase();
        return lower.startsWith("data:")
                || lower.startsWith("blob:")
                || lower.startsWith("javascript:")
                || lower.startsWith("mailto:")
                || lower.startsWith("tel:");
    }

    private String preserveOuterWhitespace(String original, String rewrittenTrimmed) {
        int prefixLength = 0;
        while (prefixLength < original.length() && Character.isWhitespace(original.charAt(prefixLength))) {
            prefixLength++;
        }
        int suffixStart = original.length();
        while (suffixStart > prefixLength && Character.isWhitespace(original.charAt(suffixStart - 1))) {
            suffixStart--;
        }
        return original.substring(0, prefixLength) + rewrittenTrimmed + original.substring(suffixStart);
    }

    private boolean isSimpleType(Class<?> type) {
        return type.isPrimitive()
                || Number.class.isAssignableFrom(type)
                || Boolean.class == type
                || Character.class == type
                || Enum.class.isAssignableFrom(type)
                || Date.class.isAssignableFrom(type)
                || Temporal.class.isAssignableFrom(type);
    }

    private boolean shouldReflect(Class<?> type) {
        Package typePackage = type.getPackage();
        String packageName = typePackage == null ? "" : typePackage.getName();
        return packageName.startsWith("cn.lili") || packageName.startsWith("com.baomidou");
    }

    private void fillLocalDefault(OssSetting setting) {
        if (setting == null || !OssEnum.LOCAL.name().equals(setting.getType())) {
            return;
        }
        if (CharSequenceUtil.isBlank(setting.getLocalFileUrlPrefix())) {
            String urlPrefix = localFileProperties == null ? DEFAULT_LOCAL_PREFIX : localFileProperties.getUrlPrefix();
            setting.setLocalFileUrlPrefix(CharSequenceUtil.blankToDefault(urlPrefix, DEFAULT_LOCAL_PREFIX));
        }
        if (CharSequenceUtil.isBlank(setting.getLocalFilePath()) && localFileProperties != null) {
            setting.setLocalFilePath(localFileProperties.getPath());
        }
    }
}
