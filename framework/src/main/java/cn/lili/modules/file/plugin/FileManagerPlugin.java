package cn.lili.modules.file.plugin;

import java.io.InputStream;
import java.util.List;

/**
 * 文件管理插件
 *
 * @author Chopper
 */
public interface FileManagerPlugin {


    /**
     * 文件路径上传
     *
     * @param filePath
     * @param key
     * @return
     */
    String pathUpload(String filePath, String key);

    /**
     * 文件流上传
     *
     * @param inputStream
     * @param key
     * @return
     */
    String inputStreamUpload(InputStream inputStream, String key);


    /**
     * 删除文件
     *
     * @param key
     */
    void deleteFile(List<String> key);

}
