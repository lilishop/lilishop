package cn.lili.modules.payment.kit.plugin.wechat;

import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.utils.StringUtils;
import cn.lili.modules.connect.entity.Connect;
import cn.lili.modules.connect.entity.enums.SourceEnum;
import cn.lili.modules.connect.service.ConnectService;
import cn.lili.modules.member.entity.dto.ConnectQueryDTO;
import cn.lili.modules.payment.kit.core.kit.HttpKit;
import cn.lili.modules.system.entity.dto.WithdrawalSetting;
import cn.lili.modules.system.entity.dto.connect.WechatConnectSetting;
import cn.lili.modules.system.entity.dto.connect.dto.WechatConnectSettingItem;
import cn.lili.modules.system.entity.dto.payment.WechatPaymentSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wallet.entity.dos.MemberWithdrawApply;
import cn.lili.modules.wallet.entity.dto.TransferResultDTO;
import cn.lili.modules.wallet.service.MemberWithdrawApplyService;
import com.google.gson.Gson;
import com.google.gson.annotations.SerializedName;
import com.wechat.pay.java.core.Config;
import com.wechat.pay.java.core.cipher.PrivacyEncryptor;
import com.wechat.pay.java.core.exception.ValidationException;
import com.wechat.pay.java.core.http.Constant;
import com.wechat.pay.java.core.http.DefaultHttpClientBuilder;
import com.wechat.pay.java.core.http.HttpClient;
import com.wechat.pay.java.core.http.HttpHeaders;
import com.wechat.pay.java.core.http.HttpMethod;
import com.wechat.pay.java.core.http.HttpRequest;
import com.wechat.pay.java.core.http.HttpResponse;
import com.wechat.pay.java.core.http.JsonRequestBody;
import com.wechat.pay.java.core.http.MediaType;
import com.wechat.pay.java.core.notification.NotificationConfig;
import com.wechat.pay.java.core.notification.NotificationParser;
import com.wechat.pay.java.core.notification.RequestParam;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import jakarta.servlet.http.HttpServletRequest;
import java.util.ArrayList;
import java.util.List;

/**
 * 微信商家转账（提现）支持组件
 * 负责会员提现发起商家转账到零钱、微信出资授权请求构建，以及转账异步回调验签与提现结果驱动。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class WechatTransferSupport {

    @Autowired
    private SettingService settingService;

    @Autowired
    private ConnectService connectService;

    @Autowired
    private MemberWithdrawApplyService memberWithdrawApplyService;

    @Autowired
    private WechatConfigSupport wechatConfigSupport;

    /**
     * 发起微信商家转账到零钱（会员提现）
     * 校验提现系统设置与联合登录绑定 OpenId，组装商户单号与转账场景信息，向微信出资接口发起转账。
     *
     * @param memberWithdrawApply 会员提现申请记录
     * @param notifyUrl           转账异步通知 URL
     * @return 提现发起结果 DTO
     */
    public TransferResultDTO transfer(MemberWithdrawApply memberWithdrawApply, String notifyUrl) {
        try {
            // 获取提现配置
            WithdrawalSetting withdrawalSetting = new Gson().fromJson(
                    settingService.get(SettingEnum.WITHDRAWAL_SETTING.name()).getSettingValue(),
                    WithdrawalSetting.class
            );
            if (withdrawalSetting == null || StringUtils.isEmpty(withdrawalSetting.getWechatAppId())) {
                return TransferResultDTO.builder().result(false).response("微信提现配置缺失：wechatAppId").build();
            }

            // 获取用户 OPENID 渠道来源
            WechatConnectSetting wechatConnectSetting = new Gson().fromJson(
                    settingService.get(SettingEnum.WECHAT_CONNECT.name()).getSettingValue(),
                    WechatConnectSetting.class
            );
            String source = "";
            for (WechatConnectSettingItem item : wechatConnectSetting.getWechatConnectSettingItems()) {
                if (item.getAppId().equals(withdrawalSetting.getWechatAppId())) {
                    switch (item.getClientType()) {
                        case "PC":
                            source = SourceEnum.WECHAT_PC_OPEN_ID.name();
                            break;
                        case "H5":
                            source = SourceEnum.WECHAT_OFFIACCOUNT_OPEN_ID.name();
                            break;
                        case "WECHAT_MP":
                            source = SourceEnum.WECHAT_MP_OPEN_ID.name();
                            break;
                        case "APP":
                            source = SourceEnum.WECHAT_APP_OPEN_ID.name();
                            break;
                        default:
                            break;
                    }
                }
            }

            // 获取用户 openId
            Connect connect = connectService.queryConnect(
                    ConnectQueryDTO.builder()
                            .userId(memberWithdrawApply.getMemberId())
                            .unionType(source)
                            .build()
            );
            if (connect == null || StringUtils.isEmpty(connect.getUnionId())) {
                return TransferResultDTO.builder().result(false).response("未获取到用户微信OpenId").build();
            }

            WechatPaymentSetting setting = wechatConfigSupport.getPaymentSetting();
            Config config = wechatConfigSupport.getWechatConfig(setting);

            MerchantTransferWithAuthorizationRequest request = new MerchantTransferWithAuthorizationRequest();
            request.setAppid(withdrawalSetting.getWechatAppId());
            request.setOutBillNo(SnowFlake.createStr("T"));
            request.setTransferSceneId("1000");
            request.setTransferRemark("用户提现");

            Long amountFen = CurrencyUtil.getFenLong(memberWithdrawApply.getApplyMoney());
            if (amountFen == null || amountFen <= 0) {
                return TransferResultDTO.builder().result(false).response("提现金额异常").build();
            }
            request.setTransferAmount(amountFen);
            request.setOpenid(connect.getUnionId());
            request.setNotifyUrl(notifyUrl);
            request.setTransferSceneReportInfos(buildWithdrawTransferSceneReportInfos());

            UserRecvStyle userRecvStyle = new UserRecvStyle();
            userRecvStyle.setType("CONFIRM_PAGE");
            request.setUserRecvStyle(userRecvStyle);

            MerchantTransferWithAuthorizationResult transferResult = createMerchantTransferWithAuthorization(config, request);
            MerchantTransferWithAuthorizationResponse response = transferResult.getResponse();
            log.info("微信提现响应 outBillNo={}, statusCode={}, response={}", request.getOutBillNo(), transferResult.getStatusCode(), response);

            if (transferResult.getStatusCode() == 200
                    && response != null
                    && "WAIT_USER_CONFIRM".equals(response.getState())) {
                return TransferResultDTO.builder()
                        .result(true)
                        .response("")
                        .outBillNo(request.getOutBillNo())
                        .wechatPackage(response.getPackageInfo())
                        .build();
            }

            boolean accepted = response != null
                    && !StringUtils.isEmpty(response.getTransferBillNo())
                    && !"FAIL".equals(response.getState())
                    && !"CANCELLED".equals(response.getState());
            return TransferResultDTO.builder().result(accepted).response(accepted ? null : JSONUtil.toJsonStr(response)).build();
        } catch (Exception e) {
            log.error("用户微信提现异常", e);
            return TransferResultDTO.builder().result(false).response(e.getMessage()).build();
        }
    }

    /**
     * 处理微信提现异步通知
     * 校验回调签名、解密提现报文，状态为 SUCCESS 时推进提现记录终态
     *
     * @param request 包含微信提现通知头的 HTTP 请求
     */
    public void transferNotify(HttpServletRequest request) {
        RequestParam requestParam = new RequestParam.Builder()
                .serialNumber(request.getHeader("Wechatpay-Serial"))
                .nonce(request.getHeader("Wechatpay-Nonce"))
                .signature(request.getHeader("Wechatpay-Signature"))
                .timestamp(request.getHeader("Wechatpay-Timestamp"))
                .body(HttpKit.readData(request))
                .build();

        NotificationConfig config = wechatConfigSupport.getNotificationConfig();
        NotificationParser parser = new NotificationParser(config);

        try {
            MerchantTransferNotifyResource transferNotifyResource = parser.parse(requestParam, MerchantTransferNotifyResource.class);
            if (transferNotifyResource == null || StringUtils.isEmpty(transferNotifyResource.getOutBillNo())) {
                log.warn("微信提现回调解析成功，但未获取到 outBillNo");
                return;
            }
            log.info("微信提现回调，outBillNo={}, state={}, transferBillNo={}",
                    transferNotifyResource.getOutBillNo(), transferNotifyResource.getState(), transferNotifyResource.getTransferBillNo());
            if ("SUCCESS".equals(transferNotifyResource.getState())) {
                memberWithdrawApplyService.transferSuccess(transferNotifyResource.getOutBillNo());
                return;
            }
            log.warn("微信提现回调为非成功终态，outBillNo={}, state={}, failReason={}",
                    transferNotifyResource.getOutBillNo(), transferNotifyResource.getState(), transferNotifyResource.getFailReason());
        } catch (ValidationException e) {
            log.error("微信提现回调验签失败", e);
        } catch (Exception e) {
            log.error("微信提现回调处理异常", e);
        }
    }

    /**
     * 组装提现转账场景上报信息
     *
     * @return 转账场景信息列表
     */
    private List<TransferSceneReportInfo> buildWithdrawTransferSceneReportInfos() {
        List<TransferSceneReportInfo> reportInfos = new ArrayList<>();
        TransferSceneReportInfo reportInfo = new TransferSceneReportInfo();
        reportInfo.setInfoType("活动名称");
        reportInfo.setInfoContent("新会员有礼");
        reportInfos.add(reportInfo);

        TransferSceneReportInfo reportInfo1 = new TransferSceneReportInfo();
        reportInfo1.setInfoType("奖励说明");
        reportInfo1.setInfoContent("奖励");
        reportInfos.add(reportInfo1);
        return reportInfos;
    }

    /**
     * 发送微信商家转账 HTTP 请求并处理加密头
     *
     * @param config  微信 SDK 配置
     * @param request 商家转账请求数据
     * @return 响应结果包装
     */
    private MerchantTransferWithAuthorizationResult createMerchantTransferWithAuthorization(Config config,
                                                                                            MerchantTransferWithAuthorizationRequest request) {
        HttpClient httpClient = new DefaultHttpClientBuilder().config(config).build();
        PrivacyEncryptor encryptor = config.createEncryptor();
        HttpHeaders headers = new HttpHeaders();
        headers.addHeader(Constant.ACCEPT, MediaType.APPLICATION_JSON.getValue());
        headers.addHeader(Constant.CONTENT_TYPE, MediaType.APPLICATION_JSON.getValue());
        headers.addHeader(Constant.WECHAT_PAY_SERIAL, encryptor.getWechatpaySerial());
        HttpRequest httpRequest = new HttpRequest.Builder()
                .httpMethod(HttpMethod.POST)
                .url("https://api.mch.weixin.qq.com/v3/fund-app/mch-transfer/transfer-bills")
                .headers(headers)
                .body(new JsonRequestBody.Builder().body(new Gson().toJson(request)).build())
                .build();
        HttpResponse<MerchantTransferWithAuthorizationResponse> response = httpClient.execute(
                httpRequest,
                MerchantTransferWithAuthorizationResponse.class
        );
        return new MerchantTransferWithAuthorizationResult(200, response.getServiceResponse());
    }

    /**
     * 微信商家转账请求参数
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Data
    public static class MerchantTransferWithAuthorizationRequest {
        @Schema(description = "商户绑定的 AppID")
        @SerializedName("appid")
        private String appid;

        @Schema(description = "商户系统内部提现单号")
        @SerializedName("out_bill_no")
        private String outBillNo;

        @Schema(description = "转账场景 ID，固定值如 1000")
        @SerializedName("transfer_scene_id")
        private String transferSceneId;

        @Schema(description = "收款用户微信 OpenID")
        @SerializedName("openid")
        private String openid;

        @Schema(description = "转账结果异步通知回调地址")
        @SerializedName("notify_url")
        private String notifyUrl;

        @Schema(description = "收款用户真实姓名（可选）")
        @SerializedName("user_name")
        private String userName;

        @Schema(description = "转账金额，单位：分")
        @SerializedName("transfer_amount")
        private Long transferAmount;

        @Schema(description = "转账备注说明")
        @SerializedName("transfer_remark")
        private String transferRemark;

        @Schema(description = "用户收款感知描述")
        @SerializedName("user_recv_perception")
        private String userRecvPerception;

        @Schema(description = "转账场景报备信息列表")
        @SerializedName("transfer_scene_report_infos")
        private List<TransferSceneReportInfo> transferSceneReportInfos;

        @Schema(description = "用户收款确认样式配置")
        @SerializedName("user_recv_style")
        private UserRecvStyle userRecvStyle;
    }

    /**
     * 用户收款确认样式配置
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Data
    public static class UserRecvStyle {
        @Schema(description = "收款确认样式类型，如 CONFIRM_PAGE")
        @SerializedName("type")
        private String type;
    }

    /**
     * 转账场景报备条目
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Data
    public static class TransferSceneReportInfo {
        @Schema(description = "场景信息类型，如：活动名称、奖励说明")
        @SerializedName("info_type")
        private String infoType;

        @Schema(description = "场景信息具体内容")
        @SerializedName("info_content")
        private String infoContent;
    }

    /**
     * 微信商家转账执行结果包装
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class MerchantTransferWithAuthorizationResult {
        @Schema(description = "HTTP 响应状态码")
        private Integer statusCode;

        @Schema(description = "微信转账接口业务响应实体")
        private MerchantTransferWithAuthorizationResponse response;
    }

    /**
     * 微信商家转账接口业务响应数据
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Data
    public static class MerchantTransferWithAuthorizationResponse {
        @Schema(description = "商户提现单号")
        @SerializedName("out_bill_no")
        private String outBillNo;

        @Schema(description = "微信转账单号")
        @SerializedName("transfer_bill_no")
        private String transferBillNo;

        @Schema(description = "创建时间")
        @SerializedName("create_time")
        private String createTime;

        @Schema(description = "转账单状态，如 WAIT_USER_CONFIRM / SUCCESS / FAIL")
        @SerializedName("state")
        private String state;

        @Schema(description = "前端调起确认收款所需的 package 串")
        @SerializedName("package_info")
        private String packageInfo;

        @Schema(description = "收款用户展示名")
        @SerializedName("user_display_name")
        private String userDisplayName;

        @Schema(description = "商户授权单号")
        @SerializedName("out_authorization_no")
        private String outAuthorizationNo;
    }

    /**
     * 微信商家转账异步通知解密资源数据
     *
     * @author Bulbasaur
     * @since 2026-11-12
     */
    @Data
    public static class MerchantTransferNotifyResource {
        @Schema(description = "商户提现单号")
        @SerializedName("out_bill_no")
        private String outBillNo;

        @Schema(description = "微信转账单号")
        @SerializedName("transfer_bill_no")
        private String transferBillNo;

        @Schema(description = "最终转账状态，如 SUCCESS / FAIL / CANCELLED")
        @SerializedName("state")
        private String state;

        @Schema(description = "转账失败原因描述")
        @SerializedName("fail_reason")
        private String failReason;
    }
}
