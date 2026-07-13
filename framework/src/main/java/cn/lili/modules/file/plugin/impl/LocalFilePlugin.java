package cn.lili.modules.file.plugin.impl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.file.entity.enums.OssEnum;
import cn.lili.modules.file.plugin.FilePlugin;
import cn.lili.modules.system.entity.dto.OssSetting;
import lombok.extern.slf4j.Slf4j;

import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;

/**
 * 本地文件存储插件。
 */
@Slf4j
public class LocalFilePlugin implements FilePlugin {

    private static final String DEFAULT_FILE_PATH = "data/uploads";

    private static final String DEFAULT_URL_PREFIX = "/files";

    private final OssSetting ossSetting;

    public LocalFilePlugin(OssSetting ossSetting) {
        this.ossSetting = ossSetting;
    }

    @Override
    public OssEnum pluginName() {
        return OssEnum.LOCAL;
    }

    @Override
    public String pathUpload(String filePath, String key) {
        try {
            return this.inputStreamUpload(new FileInputStream(filePath), key);
        } catch (Exception e) {
            log.error("本地文件上传失败", e);
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR, e.getMessage());
        }
    }

    @Override
    public String inputStreamUpload(InputStream inputStream, String key, String contentType) {
        Path target = resolveSafePath(key);
        try (InputStream source = inputStream) {
            Files.createDirectories(target.getParent());
            Files.copy(source, target, StandardCopyOption.REPLACE_EXISTING);
            return buildAccessUrl(key);
        } catch (Exception e) {
            log.error("本地文件上传失败", e);
            throw new ServiceException(ResultCode.OSS_EXCEPTION_ERROR, e.getMessage());
        }
    }

    @Override
    public void deleteFile(List<String> keys) {
        if (keys == null || keys.isEmpty()) {
            return;
        }
        for (String key : keys) {
            try {
                Files.deleteIfExists(resolveSafePath(key));
            } catch (Exception e) {
                log.error("本地文件删除失败：{}", key, e);
                throw new ServiceException(ResultCode.OSS_DELETE_ERROR, e.getMessage());
            }
        }
    }

    private Path resolveSafePath(String key) {
        Path root = Paths.get(getFilePath()).toAbsolutePath().normalize();
        Path target = root.resolve(cleanKey(key)).normalize();
        if (!target.startsWith(root)) {
            throw new ServiceException(ResultCode.FILE_TYPE_NOT_SUPPORT);
        }
        return target;
    }

    private String buildAccessUrl(String key) {
        String prefix = CharSequenceUtil.blankToDefault(ossSetting.getLocalFileUrlPrefix(), DEFAULT_URL_PREFIX);
        prefix = CharSequenceUtil.removeSuffix(prefix, "/");
        return prefix + "/" + cleanKey(key);
    }

    private String getFilePath() {
        return CharSequenceUtil.blankToDefault(ossSetting.getLocalFilePath(), DEFAULT_FILE_PATH);
    }

    private String cleanKey(String key) {
        return CharSequenceUtil.nullToEmpty(key).replace("\\", "/").replaceAll("^/+", "");
    }
}
