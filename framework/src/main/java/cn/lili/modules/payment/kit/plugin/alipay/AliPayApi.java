package cn.lili.modules.payment.kit.plugin.alipay;

import cn.hutool.http.HtmlUtil;
import com.alipay.api.*;
import com.alipay.api.domain.*;
import com.alipay.api.request.*;
import com.alipay.api.response.*;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * 支付宝支付接口门面类
 *
 * @author Chopper
 * @since 2020/12/15 19:26
 */
public class AliPayApi {

    public static <T extends AlipayResponse> T doExecute(AlipayRequest<T> request) throws AlipayApiException {
        return certificateExecute(request);
    }

    public static <T extends AlipayResponse> T doExecute(AlipayRequest<T> request, String authToken) throws AlipayApiException {
        return certificateExecute(request, authToken);
    }

    public static <T extends AlipayResponse> T execute(AlipayRequest<T> request) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().execute(request);
    }

    public static <T extends AlipayResponse> T execute(AlipayRequest<T> request, String authToken) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().execute(request, authToken);
    }

    public static <T extends AlipayResponse> T execute(AlipayRequest<T> request, String accessToken, String appAuthToken) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().execute(request, accessToken, appAuthToken);
    }

    public static <T extends AlipayResponse> T execute(AlipayRequest<T> request, String accessToken, String appAuthToken, String targetAppId) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().execute(request, accessToken, appAuthToken, targetAppId);
    }

    public static <T extends AlipayResponse> T pageExecute(AlipayRequest<T> request) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().pageExecute(request);
    }

    public static <T extends AlipayResponse> T pageExecute(AlipayRequest<T> request, String method) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().pageExecute(request, method);
    }

    public static <T extends AlipayResponse> T sdkExecute(AlipayRequest<T> request) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().sdkExecute(request);
    }

    public static BatchAlipayResponse execute(BatchAlipayRequest request) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().execute(request);
    }

    public static <T extends AlipayResponse> T certificateExecute(AlipayRequest<T> request) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().certificateExecute(request);
    }

    public static <T extends AlipayResponse> T certificateExecute(AlipayRequest<T> request, String authToken) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().certificateExecute(request, authToken);
    }

    public static <T extends AlipayResponse> T certificateExecute(AlipayRequest<T> request, String accessToken, String appAuthToken) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().certificateExecute(request, accessToken, appAuthToken);
    }

    public static <T extends AlipayResponse> T certificateExecute(AlipayRequest<T> request, String accessToken, String appAuthToken, String targetAppId) throws AlipayApiException {
        return AliPayApiConfigKit.getAliPayApiConfig().certificateExecute(request, accessToken, appAuthToken, targetAppId);
    }

    public static AlipayTradeAppPayResponse appPayToResponse(AlipayTradeAppPayModel model, String notifyUrl) throws AlipayApiException {
        return AliPayTradeApi.appPayToResponse(model, notifyUrl);
    }

    public static AlipayTradeAppPayResponse appPayToResponse(AlipayTradeAppPayModel model, String notifyUrl, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.appPayToResponse(model, notifyUrl, appAuthToken);
    }

    public static void wapPay(HttpServletResponse response, AlipayTradeWapPayModel model, String returnUrl, String notifyUrl) throws AlipayApiException, IOException {
        AliPayTradeApi.wapPay(response, model, returnUrl, notifyUrl);
    }

    public static void wapPay(HttpServletResponse response, AlipayTradeWapPayModel model, String returnUrl,
                              String notifyUrl, String appAuthToken) throws AlipayApiException, IOException {
        AliPayTradeApi.wapPay(response, model, returnUrl, notifyUrl, appAuthToken);
    }

    public static void wapPayByOutputStream(HttpServletResponse response, AlipayTradeWapPayModel model, String returnUrl, String notifyUrl, String appAuthToken) throws AlipayApiException, IOException {
        AliPayTradeApi.wapPayByOutputStream(response, model, returnUrl, notifyUrl, appAuthToken);
    }

    public static void wapPayByOutputStream(HttpServletResponse response, AlipayTradeWapPayModel model, String returnUrl, String notifyUrl) throws AlipayApiException, IOException {
        AliPayTradeApi.wapPayByOutputStream(response, model, returnUrl, notifyUrl);
    }

    public static String wapPayStr(AlipayTradeWapPayModel model, String returnUrl, String notifyUrl) throws AlipayApiException {
        return AliPayTradeApi.wapPayStr(model, returnUrl, notifyUrl);
    }

    public static String wapPayStr(AlipayTradeWapPayModel model, String returnUrl, String notifyUrl, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.wapPayStr(model, returnUrl, notifyUrl, appAuthToken);
    }

    public static AlipayTradePayResponse tradePayToResponse(AlipayTradePayModel model, String notifyUrl) throws AlipayApiException {
        return AliPayTradeApi.tradePayToResponse(model, notifyUrl);
    }

    public static AlipayTradePayResponse tradePayToResponse(AlipayTradePayModel model, String notifyUrl, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.tradePayToResponse(model, notifyUrl, appAuthToken);
    }

    public static AlipayTradePrecreateResponse tradePrecreatePayToResponse(AlipayTradePrecreateModel model, String notifyUrl) throws AlipayApiException {
        return AliPayTradeApi.tradePrecreatePayToResponse(model, notifyUrl);
    }

    public static AlipayTradePrecreateResponse tradePrecreatePayToResponse(AlipayTradePrecreateModel model,
                                                                           String notifyUrl, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.tradePrecreatePayToResponse(model, notifyUrl, appAuthToken);
    }

    @Deprecated
    public static boolean transfer(AlipayFundTransToaccountTransferModel model) throws AlipayApiException {
        return AliPayFundApi.transfer(model);
    }

    public static AlipayFundTransToaccountTransferResponse transferToResponse(AlipayFundTransToaccountTransferModel model) throws AlipayApiException {
        return AliPayFundApi.transferToResponse(model);
    }

    @Deprecated
    public static boolean transferQuery(AlipayFundTransOrderQueryModel model) throws AlipayApiException {
        return AliPayFundApi.transferQuery(model);
    }

    public static AlipayFundTransOrderQueryResponse transferQueryToResponse(AlipayFundTransOrderQueryModel model) throws AlipayApiException {
        return AliPayFundApi.transferQueryToResponse(model);
    }

    public static AlipayFundTransUniTransferResponse uniTransferToResponse(AlipayFundTransUniTransferModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.uniTransferToResponse(model, appAuthToken);
    }

    public static AlipayFundTransCommonQueryResponse transCommonQueryToResponse(AlipayFundTransCommonQueryModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.transCommonQueryToResponse(model, appAuthToken);
    }

    public static AlipayFundAccountQueryResponse accountQueryToResponse(AlipayFundAccountQueryModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.accountQueryToResponse(model, appAuthToken);
    }

    public static AlipayTradeQueryResponse tradeQueryToResponse(AlipayTradeQueryModel model) throws AlipayApiException {
        return AliPayTradeApi.tradeQueryToResponse(model);
    }

    public static AlipayTradeQueryResponse tradeQueryToResponse(AlipayTradeQueryModel model, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.tradeQueryToResponse(model, appAuthToken);
    }

    public static AlipayTradeCancelResponse tradeCancelToResponse(AlipayTradeCancelModel model, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.tradeCancelToResponse(model, appAuthToken);
    }

    public static AlipayTradeCancelResponse tradeCancelToResponse(AlipayTradeCancelModel model) throws AlipayApiException {
        return AliPayTradeApi.tradeCancelToResponse(model);
    }

    public static AlipayTradeCloseResponse tradeCloseToResponse(AlipayTradeCloseModel model, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.tradeCloseToResponse(model, appAuthToken);
    }

    public static AlipayTradeCloseResponse tradeCloseToResponse(AlipayTradeCloseModel model) throws AlipayApiException {
        return AliPayTradeApi.tradeCloseToResponse(model);
    }

    public static AlipayTradeCreateResponse tradeCreateToResponse(AlipayTradeCreateModel model, String notifyUrl) throws AlipayApiException {
        return AliPayTradeApi.tradeCreateToResponse(model, notifyUrl);
    }

    public static AlipayTradeCreateResponse tradeCreateToResponse(AlipayTradeCreateModel model, String notifyUrl, String appAuthToken) throws AlipayApiException {
        return AliPayTradeApi.tradeCreateToResponse(model, notifyUrl, appAuthToken);
    }

    public static AlipayTradeRefundResponse tradeRefundToResponse(AlipayTradeRefundModel model) throws AlipayApiException {
        return AliPayFundApi.tradeRefundToResponse(model);
    }

    public static AlipayTradeRefundResponse tradeRefundToResponse(AlipayTradeRefundModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.tradeRefundToResponse(model, appAuthToken);
    }

    public static AlipayTradePageRefundResponse tradeRefundToResponse(AlipayTradePageRefundModel model) throws AlipayApiException {
        return AliPayFundApi.tradeRefundToResponse(model);
    }

    public static AlipayTradePageRefundResponse tradeRefundToResponse(AlipayTradePageRefundModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.tradeRefundToResponse(model, appAuthToken);
    }

    public static AlipayTradeFastpayRefundQueryResponse tradeRefundQueryToResponse(AlipayTradeFastpayRefundQueryModel model) throws AlipayApiException {
        return AliPayFundApi.tradeRefundQueryToResponse(model);
    }

    public static AlipayTradeFastpayRefundQueryResponse tradeRefundQueryToResponse(AlipayTradeFastpayRefundQueryModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.tradeRefundQueryToResponse(model, appAuthToken);
    }

    public static String billDownloadUrlQuery(AlipayDataDataserviceBillDownloadurlQueryModel model) throws AlipayApiException {
        return AliPayFundApi.billDownloadUrlQuery(model);
    }

    public static AlipayDataDataserviceBillDownloadurlQueryResponse billDownloadUrlQueryToResponse(AlipayDataDataserviceBillDownloadurlQueryModel model) throws AlipayApiException {
        return AliPayFundApi.billDownloadUrlQueryToResponse(model);
    }

    public static AlipayDataDataserviceBillDownloadurlQueryResponse billDownloadUrlQueryToResponse(AlipayDataDataserviceBillDownloadurlQueryModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.billDownloadUrlQueryToResponse(model, appAuthToken);
    }

    public static AlipayTradeOrderSettleResponse tradeOrderSettleToResponse(AlipayTradeOrderSettleModel model, String appAuthToken) throws AlipayApiException {
        return AliPayFundApi.tradeOrderSettleToResponse(model, appAuthToken);
    }

    public static AlipayTradeOrderSettleResponse tradeOrderSettleToResponse(AlipayTradeOrderSettleModel model) throws AlipayApiException {
        return AliPayFundApi.tradeOrderSettleToResponse(model);
    }

    public static void tradePage(HttpServletResponse response, AlipayTradePagePayModel model, String notifyUrl, String returnUrl, String appAuthToken) throws AlipayApiException, IOException {
        AliPayTradeApi.tradePage(response, model, notifyUrl, returnUrl, appAuthToken);
    }

    public static void tradePageByOutputStream(HttpServletResponse response, AlipayTradePagePayModel model, String notifyUrl, String returnUrl) throws AlipayApiException, IOException {
        AliPayTradeApi.tradePageByOutputStream(response, model, notifyUrl, returnUrl);
    }

    public static void tradePageByOutputStream(HttpServletResponse response, AlipayTradePagePayModel model,
                                               String notifyUrl, String returnUrl, String appAuthToken) throws AlipayApiException, IOException {
        AliPayTradeApi.tradePageByOutputStream(response, model, notifyUrl, returnUrl, appAuthToken);
    }

    public static AlipayFundAuthOrderFreezeResponse authOrderFreezeToResponse(AlipayFundAuthOrderFreezeModel model) throws AlipayApiException {
        return AliPayFundApi.authOrderFreezeToResponse(model);
    }

    public static AlipayFundAuthOrderUnfreezeResponse authOrderUnfreezeToResponse(AlipayFundAuthOrderUnfreezeModel model) throws AlipayApiException {
        return AliPayFundApi.authOrderUnfreezeToResponse(model);
    }

    public static AlipayFundAuthOrderVoucherCreateResponse authOrderVoucherCreateToResponse(AlipayFundAuthOrderVoucherCreateModel model) throws AlipayApiException {
        return AliPayFundApi.authOrderVoucherCreateToResponse(model);
    }

    public static AlipayFundAuthOperationCancelResponse authOperationCancelToResponse(AlipayFundAuthOperationCancelModel model) throws AlipayApiException {
        return AliPayFundApi.authOperationCancelToResponse(model);
    }

    public static AlipayFundAuthOperationDetailQueryResponse authOperationDetailQueryToResponse(AlipayFundAuthOperationDetailQueryModel model) throws AlipayApiException {
        return AliPayFundApi.authOperationDetailQueryToResponse(model);
    }

    public static AlipayFundCouponOrderAppPayResponse fundCouponOrderAppPayToResponse(AlipayFundCouponOrderAppPayModel model) throws AlipayApiException {
        return AliPayFundApi.fundCouponOrderAppPayToResponse(model);
    }

    public static AlipayFundCouponOrderPagePayResponse fundCouponOrderPagePayToResponse(AlipayFundCouponOrderPagePayModel model) throws AlipayApiException {
        return AliPayFundApi.fundCouponOrderPagePayToResponse(model);
    }

    public static AlipayFundCouponOrderAgreementPayResponse fundCouponOrderAgreementPayToResponse(AlipayFundCouponOrderAgreementPayModel model) throws AlipayApiException {
        return AliPayFundApi.fundCouponOrderAgreementPayToResponse(model);
    }

    public static AlipayFundCouponOrderDisburseResponse fundCouponOrderDisburseToResponse(AlipayFundCouponOrderDisburseModel model) throws AlipayApiException {
        return AliPayFundApi.fundCouponOrderDisburseToResponse(model);
    }

    public static AlipayFundCouponOrderRefundResponse fundCouponOrderRefundToResponse(AlipayFundCouponOrderRefundModel model) throws AlipayApiException {
        return AliPayFundApi.fundCouponOrderRefundToResponse(model);
    }

    public static AlipayFundCouponOperationQueryResponse fundCouponOperationQueryToResponse(AlipayFundCouponOperationQueryModel model) throws AlipayApiException {
        return AliPayFundApi.fundCouponOperationQueryToResponse(model);
    }

    public static String getOauth2Url(String appId, String redirectUri) throws UnsupportedEncodingException {
        return "https://openauth.alipay.com/oauth2/appToAppAuth.htm?app_id=" + appId + "&redirect_uri=" + URLEncoder.encode(redirectUri, StandardCharsets.UTF_8);
    }

    public static AlipayOpenAuthTokenAppResponse openAuthTokenAppToResponse(AlipayOpenAuthTokenAppModel model) throws AlipayApiException {
        AlipayOpenAuthTokenAppRequest request = new AlipayOpenAuthTokenAppRequest();
        request.setBizModel(model);
        return doExecute(request);
    }

    public static AlipayOpenAuthTokenAppQueryResponse openAuthTokenAppQueryToResponse(AlipayOpenAuthTokenAppQueryModel model) throws AlipayApiException {
        AlipayOpenAuthTokenAppQueryRequest request = new AlipayOpenAuthTokenAppQueryRequest();
        request.setBizModel(model);
        return doExecute(request);
    }

    public static Map<String, String> toMap(HttpServletRequest request) {
        Map<String, String> params = new HashMap<>(16);
        Map<String, String[]> requestParams = request.getParameterMap();
        for (Iterator<String> iter = requestParams.keySet().iterator(); iter.hasNext(); ) {
            String name = iter.next();
            String[] values = requestParams.get(name);
            String valueStr = "";
            for (int i = 0; i < values.length; i++) {
                valueStr = (i == values.length - 1) ? valueStr + values[i] : valueStr + values[i] + ",";
            }
            params.put(name, HtmlUtil.unescape(valueStr));
        }
        return params;
    }

    public static AlipayTradeRoyaltyRelationBindResponse tradeRoyaltyRelationBind(
            AlipayTradeRoyaltyRelationBindModel model) throws AlipayApiException {
        return AliPayTradeApi.tradeRoyaltyRelationBind(model);
    }

    public static AlipayTradeRoyaltyRelationUnbindResponse tradeRoyaltyRelationUnBind(
            AlipayTradeRoyaltyRelationUnbindModel model) throws AlipayApiException {
        return AliPayTradeApi.tradeRoyaltyRelationUnBind(model);
    }

    public static AlipayTradeRoyaltyRelationBatchqueryResponse tradeRoyaltyRelationBatchQuery(
            AlipayTradeRoyaltyRelationBatchqueryModel model) throws AlipayApiException {
        return AliPayTradeApi.tradeRoyaltyRelationBatchQuery(model);
    }
}
