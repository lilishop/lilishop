package cn.lili.modules.system.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.KuaidiSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.mapper.LogisticsMapper;
import cn.lili.modules.system.service.LogisticsService;
import cn.lili.modules.system.service.SettingService;
import com.alibaba.fastjson.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.google.gson.Gson;
import groovy.util.logging.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 物流公司业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 8:02 下午
 */
@Slf4j
@Service
public class LogisticsServiceImpl extends ServiceImpl<LogisticsMapper, Logistics> implements LogisticsService {
    @Autowired
    private SettingService settingService;

    @Override
    public Traces getLogistic(String logisticsId, String logisticsNo) {
        try {
            return getOrderTracesByJson(logisticsId, logisticsNo);
        } catch (Exception e) {
            log.error("获取物流公司错误",e);

        }
        return null;
    }

    @Override
    public List<Logistics> getOpenLogistics() {
        LambdaQueryWrapper<Logistics> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(Logistics::getDisabled, SwitchEnum.OPEN.name());
        return this.list(queryWrapper);
    }

    /**
     * 获取物流信息
     *
     * @param logisticsId 物流公司ID
     * @param expNo       物流单号
     * @return 物流信息
     * @throws Exception
     */
    private Traces getOrderTracesByJson(String logisticsId, String expNo) throws Exception {
        Setting setting = settingService.get(SettingEnum.KUAIDI_SETTING.name());
        if (CharSequenceUtil.isBlank(setting.getSettingValue())) {
            throw new ServiceException(ResultCode.LOGISTICS_NOT_SETTING);
        }
        KuaidiSetting kuaidiSetting = new Gson().fromJson(setting.getSettingValue(), KuaidiSetting.class);

        //ID
        String EBusinessID = kuaidiSetting.getEbusinessID();

        //KEY
        String AppKey = kuaidiSetting.getAppKey();

        //请求url
        String ReqURL = kuaidiSetting.getReqURL();

        Logistics logistics = this.getById(logisticsId);

        if (logistics != null) {
            String requestData = "{'OrderCode':'','ShipperCode':'" + logistics.getCode() + "','LogisticCode':'" + expNo + "'}";
            Map<String, String> params = new HashMap<>(8);
            params.put("RequestData", urlEncoder(requestData, "UTF-8"));
            params.put("EBusinessID", EBusinessID);
            params.put("RequestType", "1002");
            String dataSign = encrypt(requestData, AppKey, "UTF-8");
            params.put("DataSign", urlEncoder(dataSign, "UTF-8"));
            params.put("DataType", "2");

            String result = sendPost(ReqURL, params);
            Map map = (Map) JSON.parse(result);
            return new Traces(logistics.getName(), expNo, (List<Map>) map.get("Traces"));
        }
        return null;
    }

    /**
     * MD5加密
     *
     * @param str     内容
     * @param charset 编码方式
     * @throws Exception
     */
    @SuppressWarnings("unused")
    private String MD5(String str, String charset) throws Exception {
        MessageDigest md = MessageDigest.getInstance("MD5");
        md.update(str.getBytes(charset));
        byte[] result = md.digest();
        StringBuffer sb = new StringBuffer(32);
        for (int i = 0; i < result.length; i++) {
            int val = result[i] & 0xff;
            if (val <= 0xf) {
                sb.append("0");
            }
            sb.append(Integer.toHexString(val));
        }
        return sb.toString().toLowerCase();
    }

    /**
     * base64编码
     *
     * @param str     内容
     * @param charset 编码方式di
     * @throws UnsupportedEncodingException
     */
    private String base64(String str, String charset) throws UnsupportedEncodingException {
        return base64Encode(str.getBytes(charset));
    }

    @SuppressWarnings("unused")
    private String urlEncoder(String str, String charset) throws UnsupportedEncodingException {
        return URLEncoder.encode(str, charset);
    }

    /**
     * 电商Sign签名生成
     *
     * @param content  内容
     * @param keyValue Appkey
     * @param charset  编码方式
     * @return DataSign签名
     * @throws UnsupportedEncodingException ,Exception
     */
    @SuppressWarnings("unused")
    private String encrypt(String content, String keyValue, String charset) throws Exception {
        if (keyValue != null) {
            return base64(MD5(content + keyValue, charset), charset);
        }
        return base64(MD5(content, charset), charset);
    }

    /**
     * 向指定 URL 发送POST方法的请求
     *
     * @param url    发送请求的 URL
     * @param params 请求的参数集合
     * @return 远程资源的响应结果
     */
    @SuppressWarnings("unused")
    private String sendPost(String url, Map<String, String> params) {
        OutputStreamWriter out = null;
        BufferedReader in = null;
        StringBuilder result = new StringBuilder();
        try {
            URL realUrl = new URL(url);
            HttpURLConnection conn = (HttpURLConnection) realUrl.openConnection();
            //发送POST请求必须设置如下两行
            conn.setDoOutput(true);
            conn.setDoInput(true);
            //POST方法
            conn.setRequestMethod("POST");
            //设置通用的请求属性
            conn.setRequestProperty("accept", "*/*");
            conn.setRequestProperty("connection", "Keep-Alive");
            conn.setRequestProperty("user-agent",
                    "Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1;SV1)");
            conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
            conn.connect();
            //获取URLConnection对象对应的输出流
            out = new OutputStreamWriter(conn.getOutputStream(), StandardCharsets.UTF_8);
            //发送请求参数
            if (params != null) {
                StringBuilder param = new StringBuilder();
                for (Map.Entry<String, String> entry : params.entrySet()) {
                    if (param.length() > 0) {
                        param.append("&");
                    }
                    param.append(entry.getKey());
                    param.append("=");
                    param.append(entry.getValue());
                }
                out.write(param.toString());
            }
            //flush输出流的缓冲
            out.flush();
            //定义BufferedReader输入流来读取URL的响应
            in = new BufferedReader(
                    new InputStreamReader(conn.getInputStream(), StandardCharsets.UTF_8));
            String line;
            while ((line = in.readLine()) != null) {
                result.append(line);
            }
        } catch (Exception e) {
            log.error("向指定 URL 发送POST方法的请求错误",e);
        }
        //使用finally块来关闭输出流、输入流
        finally {
            try {
                if (out != null) {
                    out.close();
                }
                if (in != null) {
                    in.close();
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
        return result.toString();
    }


    private static final char[] BASE64_ENCODE_CHARS = new char[]{
            'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
            'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
            'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
            'w', 'x', 'y', 'z', '0', '1', '2', '3',
            '4', '5', '6', '7', '8', '9', '+', '/'};

    public static String base64Encode(byte[] data) {
        StringBuffer sb = new StringBuffer();
        int len = data.length;
        int i = 0;
        int b1, b2, b3;
        while (i < len) {
            b1 = data[i++] & 0xff;
            if (i == len) {
                sb.append(BASE64_ENCODE_CHARS[b1 >>> 2]);
                sb.append(BASE64_ENCODE_CHARS[(b1 & 0x3) << 4]);
                sb.append("==");
                break;
            }
            b2 = data[i++] & 0xff;
            if (i == len) {
                sb.append(BASE64_ENCODE_CHARS[b1 >>> 2]);
                sb.append(BASE64_ENCODE_CHARS[((b1 & 0x03) << 4) | ((b2 & 0xf0) >>> 4)]);
                sb.append(BASE64_ENCODE_CHARS[(b2 & 0x0f) << 2]);
                sb.append("=");
                break;
            }
            b3 = data[i++] & 0xff;
            sb.append(BASE64_ENCODE_CHARS[b1 >>> 2]);
            sb.append(BASE64_ENCODE_CHARS[((b1 & 0x03) << 4) | ((b2 & 0xf0) >>> 4)]);
            sb.append(BASE64_ENCODE_CHARS[((b2 & 0x0f) << 2) | ((b3 & 0xc0) >>> 6)]);
            sb.append(BASE64_ENCODE_CHARS[b3 & 0x3f]);
        }
        return sb.toString();
    }

}
