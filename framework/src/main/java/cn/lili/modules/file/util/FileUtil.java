package cn.lili.modules.file.util;

/**
 * FileUtil
 *
 * @author Chopper
 * @version v1.0
 * 2021-11-22 11:19
 */
public class FileUtil {

    /**
     * 根据原图生成规定尺寸的图片
     *
     * @param url    连接
     * @param width  宽
     * @param height 高
     * @return
     */
    public static String getUrl(String url, Integer width, Integer height) {
        //缩略图全路径
        return url + "?x-oss-process=style/" + width + "X" + height;
    }
}
