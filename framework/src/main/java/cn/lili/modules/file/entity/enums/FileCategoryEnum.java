package cn.lili.modules.file.entity.enums;

import cn.hutool.core.text.CharSequenceUtil;

/**
 * 文件媒体分类
 *
 * @author lilishop
 */
public enum FileCategoryEnum {

    /**
     * 图片
     */
    IMAGE("图片"),

    /**
     * 视频
     */
    VIDEO("视频"),

    /**
     * 其他文件（如 PDF）
     */
    FILE("文件");

    private final String description;

    FileCategoryEnum(String description) {
        this.description = description;
    }

    public String description() {
        return description;
    }

    /**
     * 根据 MIME 类型解析文件分类
     */
    public static FileCategoryEnum fromContentType(String contentType) {
        if (CharSequenceUtil.isBlank(contentType)) {
            return FILE;
        }
        String lower = contentType.toLowerCase();
        if (lower.contains("image")) {
            return IMAGE;
        }
        if (lower.contains("video")) {
            return VIDEO;
        }
        return FILE;
    }

    /**
     * 兼容旧版按 MIME 模糊匹配的分类解析
     */
    public static FileCategoryEnum fromLegacyFileType(String fileType) {
        if (CharSequenceUtil.isBlank(fileType)) {
            return null;
        }
        String lower = fileType.toLowerCase();
        if ("image".equals(lower) || lower.contains("image")) {
            return IMAGE;
        }
        if ("video".equals(lower) || lower.contains("video")) {
            return VIDEO;
        }
        return null;
    }
}
