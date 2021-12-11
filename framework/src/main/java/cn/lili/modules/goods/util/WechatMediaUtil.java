package cn.lili.modules.goods.util;

import cn.hutool.json.JSONObject;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;

/**
 * 微信媒体工具
 *
 * @author Bulbasaur
 * @since 2021/5/19 8:02 下午
 */
@Slf4j
@Component
public class WechatMediaUtil {
    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    /**
     * 上传多媒体数据到微信服务器
     *
     * @param mediaFileUrl 来自网络上面的媒体文件地址
     * @return
     */
    public String uploadMedia(String type, String mediaFileUrl) {
        //获取token
        String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        /*
         * 上传媒体文件到微信服务器需要请求的地址
         */
        String MEDIA_URL = "https://api.weixin.qq.com/cgi-bin/media/upload?access_token=ACCESS_TOKEN&type=TYPE";

        StringBuffer resultStr = null;
        //拼装url地址
        String mediaStr = MEDIA_URL.replace("ACCESS_TOKEN", accessToken).replace("TYPE", type);
        URL mediaUrl;
        try {
            String boundary = "----WebKitFormBoundaryOYXo8heIv9pgpGjT";
            URL url = new URL(mediaStr);
            HttpURLConnection urlConn = (HttpURLConnection) url.openConnection();
            //让输入输出流开启
            urlConn.setDoInput(true);
            urlConn.setDoOutput(true);
            //使用post方式请求的时候必须关闭缓存
            urlConn.setUseCaches(false);
            //设置请求头的Content-Type属性
            urlConn.setRequestProperty("Content-Type", "multipart/form-data; boundary=" + boundary);
            urlConn.setRequestMethod("POST");
            //获取输出流，使用输出流拼接请求体
            OutputStream out = urlConn.getOutputStream();

            //读取文件的数据,构建一个GET请求，然后读取指定地址中的数据
            mediaUrl = new URL(mediaFileUrl);
            HttpURLConnection mediaConn = (HttpURLConnection) mediaUrl.openConnection();
            //设置请求方式
            mediaConn.setRequestMethod("GET");
            //设置可以打开输入流
            mediaConn.setDoInput(true);
            //获取传输的数据类型
            String contentType = mediaConn.getHeaderField("Content-Type");
            //将获取大到的类型转换成扩展名
            String fileExt = judgeType(contentType);
            //获取输入流，从mediaURL里面读取数据
            InputStream in = mediaConn.getInputStream();
            BufferedInputStream bufferedIn = new BufferedInputStream(in);
            //数据读取到这个数组里面
            byte[] bytes = new byte[1024];
            int size = 0;
            //使用outputStream流输出信息到请求体当中去
            out.write(("--" + boundary + "\r\n").getBytes());
            out.write(("Content-Disposition: form-data; name=\"media\";\r\n"
                    + "filename=\"" + (System.currentTimeMillis()) + fileExt + "\"\r\n"
                    + "Content-Type: " + contentType + "\r\n\r\n").getBytes());
            while ((size = bufferedIn.read(bytes)) != -1) {
                out.write(bytes, 0, size);
            }
            //切记，这里的换行符不能少，否则将会报41005错误
            out.write(("\r\n--" + boundary + "--\r\n").getBytes());

            bufferedIn.close();
            in.close();
            mediaConn.disconnect();

            InputStream resultIn = urlConn.getInputStream();
            InputStreamReader reader = new InputStreamReader(resultIn);
            BufferedReader bufferedReader = new BufferedReader(reader);
            String tempStr = null;
            resultStr = new StringBuffer();
            while ((tempStr = bufferedReader.readLine()) != null) {
                resultStr.append(tempStr);
            }
            bufferedReader.close();
            reader.close();
            resultIn.close();
            urlConn.disconnect();
        } catch (Exception e) {
            log.error("微信媒体上传失败", e);
        }
        assert resultStr != null;
        JSONObject jsonObject = new JSONObject(resultStr.toString());
        log.info("微信媒体上传:" + jsonObject);
        //判断是否传递成功，如果token过期则重新获取
        if (jsonObject.get("errcode") != null && ("40001").equals(jsonObject.get("errcode"))) {
            wechatAccessTokenUtil.removeAccessToken(ClientTypeEnum.WECHAT_MP);
            return this.uploadMedia(type, mediaFileUrl);
        } else if (jsonObject.get("errcode") != null) {
            throw new ServiceException(jsonObject.get("errmsg").toString());
        } else {
            return jsonObject.get("media_id").toString();
        }

    }

    /**
     * 通过传过来的contentType判断是哪一种类型
     *
     * @param contentType 获取来自连接的contentType
     * @return
     */
    public String judgeType(String contentType) {
        String fileExt = "";
        switch (contentType) {
            case "image/png":
                fileExt = ".png";
                break;
            case "image/jpeg":
                fileExt = ".jpeg";
                break;
            case "image/jpg":
                fileExt = ".jpg";
                break;
            default:
                throw new ServiceException(ResultCode.IMAGE_FILE_EXT_ERROR);
        }
        return fileExt;
    }
}
