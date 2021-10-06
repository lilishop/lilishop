package cn.lili.modules.payment.kit.plugin.wechat;

import cn.hutool.core.util.StrUtil;
import cn.hutool.http.ContentType;
import cn.lili.modules.payment.kit.core.PaymentHttpResponse;
import cn.lili.modules.payment.kit.core.enums.RequestMethodEnums;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import cn.lili.modules.payment.kit.core.kit.PayKit;
import cn.lili.modules.payment.kit.core.kit.WxPayKit;
import cn.lili.modules.payment.kit.plugin.wechat.enums.WechatApiEnum;
import cn.lili.modules.payment.kit.plugin.wechat.enums.WechatDomain;

import java.io.File;
import java.io.InputStream;
import java.security.PrivateKey;
import java.util.HashMap;
import java.util.Map;

/**
 * 微信支付相关接口
 *
 * @author Chopper
 * @since 2021/1/26 15:25
 */

public class WechatApi {

    private WechatApi() {
    }

    /**
     * 获取接口请求的 URL
     *
     * @param wechatApiEnum {@link WechatApiEnum} 支付 API 接口枚举
     * @return {@link String} 返回完整的接口请求URL
     */
    public static String getReqUrl(WechatApiEnum wechatApiEnum) {
        return getReqUrl(wechatApiEnum, null, false);
    }

    /**
     * 获取接口请求的 URL
     *
     * @param wechatApiEnum {@link WechatApiEnum} 支付 API 接口枚举
     * @param isSandBox 是否是沙箱环境
     * @return {@link String} 返回完整的接口请求URL
     */
    public static String getReqUrl(WechatApiEnum wechatApiEnum, boolean isSandBox) {
        return getReqUrl(wechatApiEnum, null, isSandBox);
    }

    /**
     * 获取接口请求的 URL
     *
     * @param wechatApiEnum    {@link WechatApiEnum} 支付 API 接口枚举
     * @param wechatDomain {@link WechatDomain} 支付 API 接口域名枚举
     * @param isSandBox    是否是沙箱环境
     * @return {@link String} 返回完整的接口请求URL
     */
    public static String getReqUrl(WechatApiEnum wechatApiEnum, WechatDomain wechatDomain, boolean isSandBox) {
        if (wechatDomain == null) {
            wechatDomain = WechatDomain.CHINA;
        }
        return wechatDomain.getType()
                .concat(isSandBox ? WechatApiEnum.SAND_BOX_NEW.getUrl() : "")
                .concat(wechatApiEnum.getUrl());
    }

    /**
     * 发起请求
     *
     * @param apiUrl 接口 URL
     *               通过 {@link WechatApi#getReqUrl(WechatApiEnum)}
     *               或者 {@link WechatApi#getReqUrl(WechatApiEnum, WechatDomain, boolean)} 来获取
     * @param params 接口请求参数
     * @return {@link String} 请求返回的结果
     */
    public static String execution(String apiUrl, Map<String, String> params) {
        return doPost(apiUrl, params);
    }

    /**
     * 发起请求
     *
     * @param apiUrl 接口 URL
     *               通过 {@link WechatApi#getReqUrl(WechatApiEnum)}
     *               或者 {@link WechatApi#getReqUrl(WechatApiEnum, WechatDomain, boolean)} 来获取
     * @param params 接口请求参数
     * @return {@link String} 请求返回的结果
     */
    public static String executionByGet(String apiUrl, Map<String, Object> params) {
        return doGet(apiUrl, params);
    }

    /**
     * 发起请求
     *
     * @param apiUrl   接口 URL
     *                 通过 {@link WechatApi#getReqUrl(WechatApiEnum)}
     *                 或者 {@link WechatApi#getReqUrl(WechatApiEnum, WechatDomain, boolean)} 来获取
     * @param params   接口请求参数
     * @param certPath 证书文件路径
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String execution(String apiUrl, Map<String, String> params, String certPath, String certPass) {
        return doPostSsl(apiUrl, params, certPath, certPass);
    }

    /**
     * 发起请求
     *
     * @param apiUrl   接口 URL
     *                 通过 {@link WechatApi#getReqUrl(WechatApiEnum)}
     *                 或者 {@link WechatApi#getReqUrl(WechatApiEnum, WechatDomain, boolean)} 来获取
     * @param params   接口请求参数
     * @param certPath 证书文件路径
     * @return {@link String} 请求返回的结果
     */
    public static String execution(String apiUrl, Map<String, String> params, String certPath) {
        return doPostSsl(apiUrl, params, certPath);
    }

    /**
     * 发起请求
     *
     * @param apiUrl   接口 URL
     *                 通过 {@link WechatApi#getReqUrl(WechatApiEnum)}
     *                 或者 {@link WechatApi#getReqUrl(WechatApiEnum, WechatDomain, boolean)} 来获取
     * @param params   接口请求参数
     * @param certFile 证书文件输入流
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String execution(String apiUrl, Map<String, String> params, InputStream certFile, String certPass) {
        return doPostSsl(apiUrl, params, certFile, certPass);
    }

    /**
     * 发起请求
     *
     * @param apiUrl   接口 URL
     *                 通过 {@link WechatApi#getReqUrl(WechatApiEnum)}
     *                 或者 {@link WechatApi#getReqUrl(WechatApiEnum, WechatDomain, boolean)} 来获取
     * @param params   接口请求参数
     * @param certFile 证书文件输入流
     * @return {@link String} 请求返回的结果
     */
    public static String execution(String apiUrl, Map<String, String> params, InputStream certFile) {
        return doPostSsl(apiUrl, params, certFile);
    }

    public static String execution(String apiUrl, Map<String, String> params,
                                   String certPath, String certPass, String filePath) {
        return doUploadSsl(apiUrl, params, certPath, certPass, filePath);
    }


    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号，接口中包含敏感信息时必传
     * @param keyPath      apiclient_key.pem 证书路径
     * @param body         接口请求参数
     * @param nonceStr     随机字符库
     * @param timestamp    时间戳
     * @param authType     认证类型
     * @param file         文件
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                         String mchId, String serialNo, String platSerialNo, String keyPath,
                                         String body, String nonceStr, long timestamp, String authType,
                                         File file) throws Exception {
        //构建 Authorization
        String authorization = WxPayKit.buildAuthorization(method, urlSuffix, mchId, serialNo,
                keyPath, body, nonceStr, timestamp, authType);

        if (StrUtil.isEmpty(platSerialNo)) {
            platSerialNo = serialNo;
        }
        if (method == RequestMethodEnums.GET) {
            return get(urlPrefix.concat(urlSuffix), authorization, platSerialNo, null);
        } else if (method == RequestMethodEnums.POST) {
            return post(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body);
        } else if (method == RequestMethodEnums.DELETE) {
            return delete(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body);
        } else if (method == RequestMethodEnums.UPLOAD) {
            return upload(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body, file);
        } else if (method == RequestMethodEnums.PUT) {
            return put(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body);
        }
        return null;
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号，接口中包含敏感信息时必传
     * @param privateKey   商户私钥
     * @param body         接口请求参数
     * @param nonceStr     随机字符库
     * @param timestamp    时间戳
     * @param authType     认证类型
     * @param file         文件
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                         String mchId, String serialNo, String platSerialNo, PrivateKey privateKey,
                                         String body, String nonceStr, long timestamp, String authType,
                                         File file) throws Exception {
        //构建 Authorization
        String authorization = WxPayKit.buildAuthorization(method, urlSuffix, mchId, serialNo,
                privateKey, body, nonceStr, timestamp, authType);

        if (StrUtil.isEmpty(platSerialNo)) {
            platSerialNo = serialNo;
        }

        if (method == RequestMethodEnums.GET) {
            return get(urlPrefix.concat(urlSuffix), authorization, platSerialNo, null);
        } else if (method == RequestMethodEnums.POST) {
            return post(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body);
        } else if (method == RequestMethodEnums.DELETE) {
            return delete(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body);
        } else if (method == RequestMethodEnums.UPLOAD) {
            return upload(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body, file);
        } else if (method == RequestMethodEnums.PUT) {
            return put(urlPrefix.concat(urlSuffix), authorization, platSerialNo, body);
        }
        return null;
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param keyPath      apiclient_key.pem 证书路径
     * @param body         接口请求参数
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(RequestMethodEnums method, String urlPrefix, String urlSuffix, String mchId,
                                         String serialNo, String platSerialNo, String keyPath, String body) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String authType = "WECHATPAY2-SHA256-RSA2048";
        String nonceStr = WxPayKit.generateStr();
        return v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, body, nonceStr, timestamp, authType, null);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param privateKey   商户私钥
     * @param body         接口请求参数
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(RequestMethodEnums method, String urlPrefix, String urlSuffix, String mchId,
                                         String serialNo, String platSerialNo, PrivateKey privateKey, String body) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String authType = "WECHATPAY2-SHA256-RSA2048";
        String nonceStr = WxPayKit.generateStr();
        return v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, privateKey, body, nonceStr, timestamp, authType, null);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param keyPath      apiclient_key.pem 证书路径
     * @param params       Get 接口请求参数
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                         String mchId, String serialNo, String platSerialNo, String keyPath,
                                         Map<String, String> params) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String authType = "WECHATPAY2-SHA256-RSA2048";
        String nonceStr = WxPayKit.generateStr();
        if (null != params && !params.keySet().isEmpty()) {
            urlSuffix = urlSuffix.concat("?").concat(PayKit.createLinkString(params, true));
        }
        return v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, "", nonceStr, timestamp, authType, null);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param privateKey   商户私钥
     * @param params       Get 接口请求参数
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                         String mchId, String serialNo, String platSerialNo, PrivateKey privateKey,
                                         Map<String, String> params) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String authType = "WECHATPAY2-SHA256-RSA2048";
        String nonceStr = WxPayKit.generateStr();
        if (null != params && !params.keySet().isEmpty()) {
            urlSuffix = urlSuffix.concat("?").concat(PayKit.createLinkString(params, true));
        }
        return v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, privateKey, "", nonceStr, timestamp, authType, null);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param keyPath      apiclient_key.pem 证书路径
     * @param body         接口请求参数
     * @param file         文件
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(String urlPrefix, String urlSuffix, String mchId, String serialNo, String platSerialNo, String keyPath, String body, File file) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String authType = "WECHATPAY2-SHA256-RSA2048";
        String nonceStr = WxPayKit.generateStr();
        return v3(RequestMethodEnums.UPLOAD, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, body, nonceStr, timestamp, authType, file);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param privateKey   商户私钥
     * @param body         接口请求参数
     * @param file         文件
     * @return {@link PaymentHttpResponse} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    public static PaymentHttpResponse v3(String urlPrefix, String urlSuffix, String mchId, String serialNo,
                                         String platSerialNo, PrivateKey privateKey, String body, File file) throws Exception {
        long timestamp = System.currentTimeMillis() / 1000;
        String authType = "WECHATPAY2-SHA256-RSA2048";
        String nonceStr = WxPayKit.generateStr();
        return v3(RequestMethodEnums.UPLOAD, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, privateKey, body, nonceStr, timestamp, authType, file);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号，接口中包含敏感信息时必传
     * @param keyPath      apiclient_key.pem 证书路径
     * @param body         接口请求参数
     * @param nonceStr     随机字符库
     * @param timestamp    时间戳
     * @param authType     认证类型
     * @param file         文件
     * @return {@link Map} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    @Deprecated
    public static Map<String, Object> v3Execution(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                                  String mchId, String serialNo, String platSerialNo, String keyPath,
                                                  String body, String nonceStr, long timestamp, String authType,
                                                  File file) throws Exception {
        PaymentHttpResponse response = v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, body, nonceStr, timestamp, authType, file);
        return buildResMap(response);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method    {@link RequestMethodEnums} 请求方法
     * @param urlPrefix 可通过 {@link WechatDomain}来获取
     * @param urlSuffix 可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId     商户Id
     * @param serialNo  商户 API 证书序列号
     * @param keyPath   apiclient_key.pem 证书路径
     * @param body      接口请求参数
     * @return {@link Map} 请求返回的结果
     */
    @Deprecated
    public static Map<String, Object> v3Execution(RequestMethodEnums method, String urlPrefix, String urlSuffix, String mchId,
                                                  String serialNo, String keyPath, String body) throws Exception {
        PaymentHttpResponse response = v3(method, urlPrefix, urlSuffix, mchId, serialNo, null, keyPath, body);
        return buildResMap(response);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param keyPath      apiclient_key.pem 证书路径
     * @param body         接口请求参数
     * @return {@link Map} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    @Deprecated
    public static Map<String, Object> v3Execution(RequestMethodEnums method, String urlPrefix, String urlSuffix, String mchId,
                                                  String serialNo, String platSerialNo, String keyPath, String body) throws Exception {
        PaymentHttpResponse response = v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, body);
        return buildResMap(response);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method       {@link RequestMethodEnums} 请求方法
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param keyPath      apiclient_key.pem 证书路径
     * @param params       Get 接口请求参数
     * @return {@link Map} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    @Deprecated
    public static Map<String, Object> v3Execution(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                                  String mchId, String serialNo, String platSerialNo, String keyPath,
                                                  Map<String, String> params) throws Exception {
        PaymentHttpResponse response = v3(method, urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, params);
        return buildResMap(response);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param method    {@link RequestMethodEnums} 请求方法
     * @param urlPrefix 可通过 {@link WechatDomain}来获取
     * @param urlSuffix 可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId     商户Id
     * @param serialNo  商户 API 证书序列号
     * @param keyPath   apiclient_key.pem 证书路径
     * @param params    Get 接口请求参数
     * @return {@link Map} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    @Deprecated
    public static Map<String, Object> v3Execution(RequestMethodEnums method, String urlPrefix, String urlSuffix,
                                                  String mchId, String serialNo, String keyPath,
                                                  Map<String, String> params) throws Exception {
        PaymentHttpResponse response = v3(method, urlPrefix, urlSuffix, mchId, serialNo, null, keyPath, params);
        return buildResMap(response);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param urlPrefix    可通过 {@link WechatDomain}来获取
     * @param urlSuffix    可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId        商户Id
     * @param serialNo     商户 API 证书序列号
     * @param platSerialNo 平台序列号
     * @param keyPath      apiclient_key.pem 证书路径
     * @param body         接口请求参数
     * @param file         文件
     * @return {@link Map} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    @Deprecated
    public static Map<String, Object> v3Upload(String urlPrefix, String urlSuffix, String mchId, String serialNo, String platSerialNo, String keyPath, String body, File file) throws Exception {
        PaymentHttpResponse response = v3(urlPrefix, urlSuffix, mchId, serialNo, platSerialNo, keyPath, body, file);
        return buildResMap(response);
    }

    /**
     * V3 接口统一执行入口
     *
     * @param urlPrefix 可通过 {@link WechatDomain}来获取
     * @param urlSuffix 可通过 {@link WechatApiEnum} 来获取，URL挂载参数需要自行拼接
     * @param mchId     商户Id
     * @param serialNo  商户 API 证书序列号
     * @param keyPath   apiclient_key.pem 证书路径
     * @param body      接口请求参数
     * @param file      文件
     * @return {@link Map} 请求返回的结果
     * @throws Exception 接口执行异常
     */
    @Deprecated
    public static Map<String, Object> v3Upload(String urlPrefix, String urlSuffix, String mchId, String serialNo, String keyPath, String body, File file) throws Exception {
        return v3Upload(urlPrefix, urlSuffix, mchId, serialNo, null, keyPath, body, file);
    }

    /**
     * 发放企业红包
     *
     * @param params   请求参数
     * @param certPath 证书文件路径
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String sendWorkWxRedPack(Map<String, String> params, String certPath, String certPass) {
        return execution(getReqUrl(WechatApiEnum.SEND_WORK_WX_RED_PACK), params, certPath, certPass);
    }

    /**
     * 发放企业红包
     *
     * @param params   请求参数
     * @param certFile 证书文件的 InputStream
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String sendWorkWxRedPack(Map<String, String> params, InputStream certFile, String certPass) {
        return execution(getReqUrl(WechatApiEnum.SEND_WORK_WX_RED_PACK), params, certFile, certPass);
    }

    /**
     * 查询向员工付款记录
     *
     * @param params   请求参数
     * @param certPath 证书文件路径
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String queryWorkWxRedPack(Map<String, String> params, String certPath, String certPass) {
        return execution(getReqUrl(WechatApiEnum.QUERY_WORK_WX_RED_PACK), params, certPath, certPass);
    }

    /**
     * 查询向员工付款记录
     *
     * @param params   请求参数
     * @param certFile 证书文件的 InputStream
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String queryWorkWxRedPack(Map<String, String> params, InputStream certFile, String certPass) {
        return execution(getReqUrl(WechatApiEnum.QUERY_WORK_WX_RED_PACK), params, certFile, certPass);
    }

    /**
     * 向员工付款
     *
     * @param params   请求参数
     * @param certPath 证书文件路径
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String trans2pocket(Map<String, String> params, String certPath, String certPass) {
        return execution(getReqUrl(WechatApiEnum.PAY_WWS_TRANS_2_POCKET), params, certPath, certPass);
    }

    /**
     * 向员工付款
     *
     * @param params   请求参数
     * @param certFile 证书文件的 InputStream
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String trans2pocket(Map<String, String> params, InputStream certFile, String certPass) {
        return execution(getReqUrl(WechatApiEnum.PAY_WWS_TRANS_2_POCKET), params, certFile, certPass);
    }

    /**
     * 查询向员工付款记录
     *
     * @param params   请求参数
     * @param certPath 证书文件路径
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String queryTrans2pocket(Map<String, String> params, String certPath, String certPass) {
        return execution(getReqUrl(WechatApiEnum.QUERY_WWS_TRANS_2_POCKET), params, certPath, certPass);
    }

    /**
     * 查询向员工付款记录
     *
     * @param params   请求参数
     * @param certFile 证书文件的 InputStream
     * @param certPass 证书密码
     * @return {@link String} 请求返回的结果
     */
    public static String queryTrans2pocket(Map<String, String> params, InputStream certFile, String certPass) {
        return execution(getReqUrl(WechatApiEnum.QUERY_WWS_TRANS_2_POCKET), params, certFile, certPass);
    }

    /**
     * @param url    请求url
     * @param params 请求参数
     * @return {@link String}    请求返回的结果
     */
    public static String doGet(String url, Map<String, Object> params) {
        return HttpKit.getDelegate().get(url, params);
    }

    /**
     * get 请求
     *
     * @param url     请求url
     * @param params  请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse get(String url, Map<String, Object> params, Map<String, String> headers) {
        return HttpKit.getDelegate().get(url, params, headers);
    }

    /**
     * get 请求
     *
     * @param url           请求url
     * @param authorization 授权信息
     * @param serialNumber  公钥证书序列号
     * @param params        请求参数
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse get(String url, String authorization, String serialNumber, Map<String, Object> params) {
        return get(url, params, getHeaders(authorization, serialNumber));
    }

    /**
     * post 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse post(String url, String data, Map<String, String> headers) {
        return HttpKit.getDelegate().post(url, data, headers);
    }

    /**
     * post 请求
     *
     * @param url           请求url
     * @param authorization 授权信息
     * @param serialNumber  公钥证书序列号
     * @param data          请求参数
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse post(String url, String authorization, String serialNumber, String data) {
        return post(url, data, getHeaders(authorization, serialNumber));
    }

    /**
     * delete 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse delete(String url, String data, Map<String, String> headers) {
        return HttpKit.getDelegate().delete(url, data, headers);
    }

    /**
     * delete 请求
     *
     * @param url           请求url
     * @param authorization 授权信息
     * @param serialNumber  公钥证书序列号
     * @param data          请求参数
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse delete(String url, String authorization, String serialNumber, String data) {
        return delete(url, data, getHeaders(authorization, serialNumber));
    }

    /**
     * upload 请求
     *
     * @param url     请求url
     * @param params  请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse upload(String url, Map<String, Object> params, Map<String, String> headers) {
        return HttpKit.getDelegate().post(url, params, headers);
    }

    /**
     * upload 请求
     *
     * @param url           请求url
     * @param authorization 授权信息
     * @param serialNumber  公钥证书序列号
     * @param data          请求参数
     * @param file          上传文件
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse upload(String url, String authorization, String serialNumber, String data, File file) {
        Map<String, Object> paramMap = new HashMap<>(2);
        paramMap.put("file", file);
        paramMap.put("meta", data);
        return upload(url, paramMap, getUploadHeaders(authorization, serialNumber));
    }


    /**
     * put 请求
     *
     * @param url     请求url
     * @param data    请求参数
     * @param headers 请求头
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse put(String url, String data, Map<String, String> headers) {
        return HttpKit.getDelegate().put(url, data, headers);
    }

    /**
     * put 请求
     *
     * @param url           请求url
     * @param authorization 授权信息
     * @param serialNumber  公钥证书序列号
     * @param data          请求参数
     * @return {@link PaymentHttpResponse}    请求返回的结果
     */
    public static PaymentHttpResponse put(String url, String authorization, String serialNumber, String data) {
        return put(url, data, getHeaders(authorization, serialNumber));
    }

    public static String doPost(String url, Map<String, String> params) {
        return HttpKit.getDelegate().post(url, WxPayKit.toXml(params));
    }

    public static String doPostSsl(String url, Map<String, String> params, String certPath, String certPass) {
        return HttpKit.getDelegate().post(url, WxPayKit.toXml(params), certPath, certPass);
    }

    public static String doPostSsl(String url, Map<String, String> params, InputStream certFile, String certPass) {
        return HttpKit.getDelegate().post(url, WxPayKit.toXml(params), certFile, certPass);
    }

    public static String doPostSsl(String url, Map<String, String> params, String certPath) {
        if (params.isEmpty() || !params.containsKey("mch_id")) {
            throw new RuntimeException("请求参数中必须包含 mch_id，如接口参考中不包 mch_id， 请使用其他同名构造方法。");
        }
        String certPass = params.get("mch_id");
        return doPostSsl(url, params, certPath, certPass);
    }

    public static String doPostSsl(String url, Map<String, String> params, InputStream certFile) {
        if (params.isEmpty() || !params.containsKey("mch_id")) {
            throw new RuntimeException("请求参数中必须包含 mch_id，如接口参考中不包 mch_id， 请使用其他同名构造方法。");
        }
        String certPass = params.get("mch_id");
        return doPostSsl(url, params, certFile, certPass);
    }

    public static String doUploadSsl(String url, Map<String, String> params, String certPath, String certPass, String filePath) {
        return HttpKit.getDelegate().upload(url, WxPayKit.toXml(params), certPath, certPass, filePath);
    }

    public static String doUploadSsl(String url, Map<String, String> params, String certPath, String filePath) {
        if (params.isEmpty() || !params.containsKey("mch_id")) {
            throw new RuntimeException("请求参数中必须包含 mch_id，如接口参考中不包 mch_id， 请使用其他同名构造方法。");
        }
        String certPass = params.get("mch_id");
        return doUploadSsl(url, params, certPath, certPass, filePath);
    }


    public static Map<String, String> getBaseHeaders(String authorization) {
        Map<String, String> headers = new HashMap<>(5);
        headers.put("Accept", ContentType.JSON.toString());
        headers.put("Authorization", authorization);
        return headers;
    }

    public static Map<String, String> getHeaders(String authorization, String serialNumber) {
        Map<String, String> headers = getBaseHeaders(authorization);
        headers.put("Content-Type", ContentType.JSON.toString());
        if (StrUtil.isNotEmpty(serialNumber)) {
            headers.put("Wechatpay-Serial", serialNumber);
        }
        return headers;
    }

    public static Map<String, String> getUploadHeaders(String authorization, String serialNumber) {
        Map<String, String> headers = getBaseHeaders(authorization);
        headers.put("Content-Type", "multipart/form-data;boundary=\"boundary\"");
        if (StrUtil.isNotEmpty(serialNumber)) {
            headers.put("Wechatpay-Serial", serialNumber);
        }
        return headers;
    }

    /**
     * 构建返回参数
     *
     * @param response {@link PaymentHttpResponse}
     * @return {@link Map}
     */
    public static Map<String, Object> buildResMap(PaymentHttpResponse response) {
        if (response == null) {
            return null;
        }
        Map<String, Object> map = new HashMap<>(6);
        String timestamp = response.getHeader("Wechatpay-Timestamp");
        String nonceStr = response.getHeader("Wechatpay-Nonce");
        String serialNo = response.getHeader("Wechatpay-Serial");
        String signature = response.getHeader("Wechatpay-Signature");
        String body = response.getBody();
        int status = response.getStatus();
        map.put("timestamp", timestamp);
        map.put("nonceStr", nonceStr);
        map.put("serialNumber", serialNo);
        map.put("signature", signature);
        map.put("body", body);
        map.put("status", status);
        return map;
    }
}
