package cn.lili.modules.wechat.util;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.HttpUtils;
import cn.lili.modules.message.entity.dos.ShortLink;
import cn.lili.modules.message.service.ShortLinkService;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.protocol.HTTP;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Base64;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * 微信API交互token
 *
 * @author Chopper
 * @version v1.0
 * 2020-12-10 19:25
 */
@Slf4j
@Component
public class WechatMpCodeUtil {

    private static String UN_LIMIT_API = "https://api.weixin.qq.com/wxa/getwxacodeunlimit?access_token=";
    private static String CREATE_QR_CODE = "https://api.weixin.qq.com/cgi-bin/wxaapp/createwxaqrcode?access_token=";
    private static String URL_LINK_API = "https://api.weixin.qq.com/wxa/generate_urllink?access_token=";
    private static final String MP_URL_LINK_CACHE_PREFIX = "MP_URL_LINK:";

    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;
    @Autowired
    private ShortLinkService shortLinkService;
    @Autowired
    private Cache<String> cache;

    /**
     * 小程序 URL Link 打开版本：release / trial / develop / auto（依次尝试）
     */
    @Value("${lili.wechat.mp.url-link-env-version:auto}")
    private String urlLinkEnvVersion;

    /**
     * 生成分享二维码
     *
     * @param path 路径
     * @return
     */
    public String createQrCode(String path) {
        try {
            String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
            Map<String, Object> params = new HashMap<>();
            params.put("path", path);
            params.put("width", 430);
            CloseableHttpClient httpClient = HttpClients.createDefault();
            HttpPost httpPost = new HttpPost(CREATE_QR_CODE + accessToken);
            httpPost.addHeader(HTTP.CONTENT_TYPE, "application/json");
            //必须是json模式的 post
            String body = JSONUtil.toJsonStr(params);
            StringEntity entity = new StringEntity(body);
            entity.setContentType("image/png");
            httpPost.setEntity(entity);
            HttpResponse httpResponse = httpClient.execute(httpPost);
            HttpEntity httpEntity = httpResponse.getEntity();
            //======================================================================//
            //处理HTTP返回结果
            //======================================================================//
            InputStream contentStream = httpEntity.getContent();
            byte[] bytes = toByteArray(contentStream);
            contentStream.read(bytes);
            //返回内容
            return Base64.getEncoder().encodeToString(bytes);
        } catch (Exception e) {
            log.error("生成二维码错误：", e);
            throw new ServiceException(ResultCode.WECHAT_QRCODE_ERROR);
        }

    }

    /**
     * 生成分享二维码
     *
     * @param page
     * @param scene
     * @return
     */
    public String createCode(String page, String scene) {
        try {

            //短链接存储
            ShortLink shortLink = new ShortLink();
            shortLink.setOriginalParams(scene);
            List<ShortLink> shortLinks = shortLinkService.queryShortLinks(shortLink);
            if (shortLinks.size() > 0) {
                shortLink = shortLinks.get(0);
            } else {
                shortLink.setOriginalParams(scene);
                shortLinkService.save(shortLink);
                shortLink = shortLinkService.queryShortLinks(shortLink).get(0);
            }
            String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
            Map<String, Object> params = new HashMap<>(4);
            params.put("page", page);
            params.put("scene", shortLink.getId());
            params.put("width", "280");

            //======================================================================//
            //执行URL Post调用
            //======================================================================//
            CloseableHttpClient httpClient = HttpClients.createDefault();
            HttpPost httpPost = new HttpPost(UN_LIMIT_API + accessToken);
            httpPost.addHeader(HTTP.CONTENT_TYPE, "application/json");
            //必须是json模式的 post
            String body = JSONUtil.toJsonStr(params);
            StringEntity entity = new StringEntity(body);
            entity.setContentType("image/png");
            httpPost.setEntity(entity);
            HttpResponse httpResponse = httpClient.execute(httpPost);
            HttpEntity httpEntity = httpResponse.getEntity();
            //======================================================================//
            //处理HTTP返回结果
            //======================================================================//
            InputStream contentStream = httpEntity.getContent();
            byte[] bytes = toByteArray(contentStream);
            contentStream.read(bytes);
            //返回内容
            return Base64.getEncoder().encodeToString(bytes);
        } catch (Exception e) {
            log.error("生成二维码错误：", e);
            throw new ServiceException(ResultCode.WECHAT_QRCODE_ERROR);
        }

    }

    /**
     * 生成小程序 URL Link（wxmpurl.cn）
     *
     * @param path  页面路径，如 pages/product/goods
     * @param query 页面参数，如 id=1&goodsId=2
     * @return 小程序短链，失败返回 null
     */
    public String createUrlLink(String path, String query) {
        if (CharSequenceUtil.isBlank(path)) {
            return null;
        }
        String normalizedPath = path.startsWith("/") ? path.substring(1) : path;
        String normalizedQuery = CharSequenceUtil.blankToDefault(query, "");
        String cacheKey = MP_URL_LINK_CACHE_PREFIX + normalizedPath + "?" + normalizedQuery;
        try {
            String cached = cache.getString(cacheKey);
            if (CharSequenceUtil.isNotBlank(cached)) {
                return cached;
            }
        } catch (Exception e) {
            log.warn("读取小程序 URL Link 缓存失败", e);
        }
        try {
            String urlLink = requestUrlLink(normalizedPath, normalizedQuery);
            if (CharSequenceUtil.isNotBlank(urlLink)) {
                cache.put(cacheKey, urlLink, 29L, TimeUnit.DAYS);
                return urlLink;
            }
        } catch (Exception e) {
            log.error("生成小程序 URL Link 异常", e);
        }
        return null;
    }

    /**
     * 请求微信生成 URL Link，按配置依次尝试不同小程序版本。
     *
     * @param normalizedPath  不含前导斜杠的页面路径
     * @param normalizedQuery 页面查询参数，可为空
     * @return 成功时返回 url_link，失败返回 null
     */
    private String requestUrlLink(String normalizedPath, String normalizedQuery) {
        String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.WECHAT_MP);
        if (CharSequenceUtil.isBlank(accessToken)) {
            log.error("生成小程序 URL Link 失败：未获取到 WECHAT_MP access_token，请检查管理后台「联合登录/微信连接」中小程序 AppID、AppSecret 是否配置正确");
            return null;
        }
        for (String envVersion : resolveUrlLinkEnvVersions()) {
            // 某一版本成功即返回，避免重复请求
            String urlLink = doCreateUrlLink(accessToken, normalizedPath, normalizedQuery, envVersion);
            if (CharSequenceUtil.isNotBlank(urlLink)) {
                log.info("生成小程序 URL Link 成功，env_version={}", envVersion);
                return urlLink;
            }
        }
        return null;
    }

    /**
     * 解析 URL Link 请求使用的小程序版本列表。
     * <p>配置为 auto 时依次尝试 release → trial → develop。</p>
     *
     * @return 待尝试的 env_version 数组
     */
    private String[] resolveUrlLinkEnvVersions() {
        if (CharSequenceUtil.isBlank(urlLinkEnvVersion) || "auto".equalsIgnoreCase(urlLinkEnvVersion.trim())) {
            return new String[]{"release", "trial", "develop"};
        }
        return new String[]{urlLinkEnvVersion.trim()};
    }

    /**
     * 调用微信 generate_urllink 接口生成指定版本的小程序短链。
     *
     * @param accessToken       小程序 access_token
     * @param normalizedPath    页面路径
     * @param normalizedQuery   页面参数
     * @param envVersion        小程序版本：release / trial / develop
     * @return 成功时返回 url_link，失败返回 null
     */
    private String doCreateUrlLink(String accessToken, String normalizedPath, String normalizedQuery, String envVersion) {
        try {
            Map<String, Object> params = new HashMap<>(8);
            params.put("path", normalizedPath);
            if (CharSequenceUtil.isNotBlank(normalizedQuery)) {
                params.put("query", normalizedQuery);
            }
            // 按天过期，最长 30 天（与缓存 29 天对齐）
            params.put("expire_type", 1);
            params.put("expire_interval", 30);
            params.put("env_version", envVersion);
            String response = HttpUtils.doPostWithJson(URL_LINK_API + accessToken, params);
            if (CharSequenceUtil.isBlank(response)) {
                log.error("生成小程序 URL Link 失败：微信接口无响应，env_version={}", envVersion);
                return null;
            }
            cn.hutool.json.JSONObject json = JSONUtil.parseObj(response);
            if (json.getInt("errcode", -1) == 0 && CharSequenceUtil.isNotBlank(json.getStr("url_link"))) {
                return json.getStr("url_link");
            }
            log.error("生成小程序 URL Link 失败，env_version={}，微信返回：{}", envVersion, response);
        } catch (Exception e) {
            log.error("生成小程序 URL Link 异常，env_version={}", envVersion, e);
        }
        return null;
    }

    /**
     * @param input
     * @return
     * @throws IOException
     */
    @SneakyThrows
    public static byte[] toByteArray(InputStream input) {
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        copy(input, output);
        return output.toByteArray();
    }

    /**
     * @param input
     * @param output
     * @return
     * @throws IOException
     */
    public static int copy(InputStream input, OutputStream output) throws IOException {
        long count = copyLarge(input, output);
        if (count > 2147483647L) {
            return -1;
        }
        return (int) count;
    }

    /**
     * @param input
     * @param output
     * @return
     * @throws IOException
     */
    public static long copyLarge(InputStream input, OutputStream output) throws IOException {
        byte[] buffer = new byte[4096];
        long count = 0L;
        int n = 0;
        while (-1 != (n = input.read(buffer))) {
            output.write(buffer, 0, n);
            count += n;
        }
        return count;
    }


}