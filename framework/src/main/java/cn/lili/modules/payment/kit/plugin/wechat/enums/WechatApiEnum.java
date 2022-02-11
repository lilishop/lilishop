package cn.lili.modules.payment.kit.plugin.wechat.enums;

/**
 * 微信api列表
 *
 * @author Chopper
 * @since 2020/12/17 17:43
 */
public enum WechatApiEnum {

    /**
     * 沙箱环境
     */
    SAND_BOX_NEW("/sandboxnew"),
    /**
     * 获取沙箱环境验签秘钥
     */
    GET_SIGN_KEY("/sandboxnew/pay/getsignkey"),
    /**
     * 获取平台证书列表
     */
    GET_CERTIFICATES("/v3/certificates"),

    /**
     * 营销专用-图片上传
     */
    MARKETING_UPLOAD_MEDIA("/v3/marketing/favor/media/image-upload"),
    /**
     * 通用接口-图片上传
     */
    MERCHANT_UPLOAD_MEDIA("/v3/merchant/media/upload"),
    /**
     * 通用接口-视频上传
     */
    MERCHANT_VIDEO_UPLOAD("/v3/merchant/media/video_upload"),


    /**
     * 创建/查询支付分订单
     */
    PAY_SCORE_SERVICE_ORDER("/v3/payscore/serviceorder"),
    /**
     * 取消支付分订单
     */
    PAY_SCORE_SERVICE_ORDER_CANCEL("/v3/payscore/serviceorder/%s/cancel"),
    /**
     * 修改支付分订单金额
     */
    PAY_SCORE_SERVICE_ORDER_MODIFY("/v3/payscore/serviceorder/%s/modify"),
    /**
     * 完结支付分订单
     */
    PAY_SCORE_SERVICE_ORDER_COMPLETE("/v3/payscore/serviceorder/%s/complete"),
    /**
     * 支付分订单收款
     */
    PAY_SCORE_SERVICE_ORDER_PAY("/v3/payscore/serviceorder/%s/pay"),
    /**
     * 同步服务订单信息
     */
    PAY_SCORE_SERVICE_ORDER_SYNC("/v3/payscore/serviceorder/%s/sync"),
    /**
     * 查询用户支付分开启状态
     */
    PAY_SCORE_USER_SERVICE_STATE("/v3/payscore/user-service-state"),
    /**
     * 商户解除用户授权关系
     */
    PAY_SCORE_PERMISSIONS_TERMINATE("/payscore/users/%s/permissions/%s/terminate"),

    /**
     * 特约商户进件-提交申请单
     */
    APPLY_4_SUB("/v3/applyment4sub/applyment/"),
    /**
     * 特约商户进件-通过业务申请编号查询申请状态
     */
    GET_APPLY_STATE("/v3/applyment4sub/applyment/business_code/%s"),
    /**
     * 特约商户进件-通过申请单号查询申请状态
     */
    GET_APPLY_STATE_BY_ID("/v3/applyment4sub/applyment/applyment_id/%s"),
    /**
     * 特约商户进件-修改结算帐号
     */
    MODIFY_SETTLEMENT("/v3/apply4sub/sub_merchants/%s/modify-settlement"),
    /**
     * 特约商户进件-查询结算账户
     */
    GET_SETTLEMENT("/v3/apply4sub/sub_merchants/%s/settlement"),

    /**
     * 商户开户意愿确认-提交申请单 OR 查询申请单审核结果
     */
    MER_OPEN_APPLY_SUBMIT_OR_RESULT("/v3/apply4subject/applyment"),
    /**
     * 商户开户意愿确认-撤销申请单
     */
    MER_OPEN_APPLY_CANCEL("/v3/apply4subject/applyment/%s/cancel"),
    /**
     * 商户开户意愿确认-获取商户开户意愿确认状态
     */
    GET_MER_OPEN_APPLY_STATE("/v3/apply4subject/applyment/merchants/%s/state"),

    /**
     * 商业支付投诉-查询投诉信息
     */
    MERCHANT_SERVICE_COMPLAINTS("/v3/merchant-service/complaints"),
    /**
     * 商业支付投诉-创建/查询/更新/删除投诉通知回调
     */
    MERCHANT_SERVICE_COMPLAINTS_NOTIFICATIONS("/v3/merchant-service/complaint-notifications"),


    /**
     * 代金券-创建代金券批次
     */
    CREATE_COUPON_STOCKS("/v3/marketing/favor/coupon-stocks"),
    /**
     * 代金券-激活代金券批次
     */
    START_COUPON_STOCKS("/v3/marketing/favor/stocks/%s/start"),
    /**
     * 代金券-发放代金券
     */
    COUPON_SEND("/v3/marketing/favor/users/%s/coupons"),
    /**
     * 代金券-暂停代金券批次
     */
    PAUSE_COUPON_STOCKS("/v3/marketing/favor/stocks/%s/pause"),
    /**
     * 代金券-重启代金券批次
     */
    RESTART_COUPON_STOCKS("/v3/marketing/favor/stocks/%s/restart"),
    /**
     * 代金券-条件查询批次列表
     */
    QUERY_COUPON_STOCKS("/v3/marketing/favor/stocks"),
    /**
     * 代金券-查询批次详情
     */
    QUERY_COUPON_STOCKS_INFO("/v3/marketing/favor/stocks/%s"),
    /**
     * 代金券-查询代金券详情
     */
    QUERY_COUPON_INFO("/v3/marketing/favor/users/%s/coupons/%s"),
    /**
     * 代金券-查询代金券可用商户
     */
    QUERY_COUPON_MERCHANTS("/v3/marketing/favor/stocks/%s/merchants"),
    /**
     * 代金券-查询代金券可用单品
     */
    QUERY_COUPON_ITEMS("/v3/marketing/favor/stocks/%s/items"),
    /**
     * 代金券-根据商户号查用户的券
     */
    QUERY_USER_COUPON("/v3/marketing/favor/users/%s/coupons"),
    /**
     * 代金券-下载批次核销明细
     */
    COUPON_STOCKS_USER_FLOW_DOWNLOAD("/v3/marketing/favor/stocks/%s/use-flow"),
    /**
     * 代金券-下载批次退款明细
     */
    COUPON_STOCKS_REFUND_FLOW_DOWNLOAD("/v3/marketing/favor/stocks/%s/refund-flow"),
    /**
     * 代金券-设置消息通知地址
     */
    SETTING_COUPON_CALLBACKS("/v3/marketing/favor/callbacks"),
    /**
     * 商家券-创建商家券
     */
    CREATE_BUSINESS_COUPON("/v3/marketing/busifavor/stocks"),
    /**
     * 发放消费卡
     */
    SEND_BUSINESS_COUPON("/v3/marketing/busifavor/coupons/%s/send"),
    /**
     * H5 发券
     */
    H5_SEND_COUPON("/busifavor/getcouponinfo"),
    /**
     * 商家券-查询商家券批次详情
     */
    QUERY_BUSINESS_COUPON_STOCKS_INFO("/v3/marketing/busifavor/stocks/%s"),
    /**
     * 商家券-查询商家券批次详情
     */
    USE_BUSINESS_COUPON("/v3/marketing/busifavor/coupons/use"),
    /**
     * 商家券-根据过滤条件查询用户券
     */
    QUERY_BUSINESS_USER_COUPON("/v3/marketing/busifavor/users/%s/coupons"),
    /**
     * 商家券-查询用户单张券详情
     */
    QUERY_BUSINESS_USER_COUPON_INFO("/v3/marketing/busifavor/users/%s/coupons/%s/appids/%s"),
    /**
     * 商家券-上传预存code
     */
    BUSINESS_COUPON_UPLOAD_CODE("/v3/marketing/busifavor/stocks/%/couponcodes"),
    /**
     * 商家券-设置/查询商家券事件通知地址
     */
    BUSINESS_COUPON_CALLBACKS("/v3/marketing/busifavor/callbacks"),
    /**
     * 关联订单信息
     */
    BUSINESS_COUPON_ASSOCIATE("/v3/marketing/busifavor/coupons/associate"),
    /**
     * 取消关联订单信息
     */
    BUSINESS_COUPON_DISASSOCIATE("/v3/marketing/busifavor/coupons/disassociate"),

    /**
     * 支付有礼-创建全场满额送活动
     */
    PAY_GIFT_ACTIVITY("/v3/marketing/paygiftactivity/unique-threshold-activity"),
    /**
     * 支付有礼-获取支付有礼活动列表
     */
    PAY_GIFT_ACTIVITY_GET("/v3/marketing/paygiftactivity/activities"),
    /**
     * 支付有礼-查询活动详情接口
     */
    PAY_GIFT_ACTIVITY_INFO("/v3/marketing/paygiftactivity/activities/%s"),
    /**
     * 支付有礼-查询活动发券商户号
     */
    PAY_GIFT_ACTIVITY_QUERY_MER("/v3/marketing/paygiftactivity/activities/%s/merchants"),
    /**
     * 支付有礼-查询活动指定商品列表
     */
    PAY_GIFT_ACTIVITY_QUERY_GOODS("/v3/marketing/paygiftactivity/activities/%s/goods"),
    /**
     * 支付有礼-终止活动
     */
    PAY_GIFT_ACTIVITY_TERMINATE("/v3/marketing/paygiftactivity/activities/%s/terminate"),
    /**
     * 支付有礼-新增活动发券商户号
     */
    PAY_GIFT_ACTIVITY_ADD_MERCHANTS("/v3/marketing/paygiftactivity/activities/%s/merchants/add"),
    /**
     * 支付有礼-删除活动发券商户号
     */
    PAY_GIFT_ACTIVITY_DELETE_MERCHANTS("/v3/marketing/paygiftactivity/activities/%s/merchants/delete"),


    /**
     * 点金计划-点金计划管理
     */
    CHANGE_GOLD_PLAN_STATUS("/v3/goldplan/merchants/changegoldplanstatus"),
    /**
     * 点金计划-商家小票管理
     */
    CHANGE_CUSTOM_PAGE_STATUS("/v3/goldplan/merchants/changecustompagestatus"),
    /**
     * 点金计划-同业过滤标签管理
     */
    SET_ADVERTISING_INDUSTRY_FILTER("/v3/goldplan/merchants/set-advertising-industry-filter"),


    /**
     * 电商收付通-二级商户进件
     */
    E_COMMERCE_APPLY("/v3/ecommerce/applyments/"),
    /**
     * 电商收付通-查询进件申请状态
     */
    E_COMMERCE_APPLY_STATE("/v3/ecommerce/applyments/%s"),
    /**
     * 电商收付通-通过业务申请编号查询申请状态
     */
    E_COMMERCE_APPLY_STATE_BY_NO("/v3/ecommerce/applyments/out-request-no/%s"),

    /**
     * 合单下单-APP支付
     */
    COMBINE_TRANSACTIONS_APP("/v3/combine-transactions/app"),
    /**
     * 合单下单-JS支付
     */
    COMBINE_TRANSACTIONS_JS("/v3/combine-transactions/jsapi"),
    /**
     * 合单下单-H5支付
     */
    COMBINE_TRANSACTIONS_H5("/v3/combine-transactions/h5"),
    /**
     * 合单下单-Native支付
     */
    COMBINE_TRANSACTIONS_NATIVE("/v3/combine-transactions/native"),
    /**
     * 合单下单-合单查询订单
     */
    COMBINE_TRANSACTIONS_QUERY("/v3/combine-transactions/out-trade-no/%s"),
    /**
     * 合单下单-合单关闭订单
     */
    COMBINE_TRANSACTIONS_CLOSE("/v3/combine-transactions/out-trade-no/%s/close"),

    /**
     * 电商收付通-补差接口-请求补差
     */
    CREATE_SUBSIDIES("v3/ecommerce/subsidies/create"),
    /**
     * 电商收付通-补差接口-请求补差回退
     */
    RETURN_SUBSIDIES("/v3/ecommerce/subsidies/return"),
    /**
     * 电商收付通-补差接口-取消补差
     */
    CANCEL_SUBSIDIES("/v3/ecommerce/subsidies/cancel"),
    /**
     * 电商收付通-分账接口-请求分账/查询分账结果
     */
    PROFIT_SHARING_ORDERS("/v3/ecommerce/profitsharing/orders"),
    /**
     * 电商收付通-分账接口-查询分账回退结果
     */
    PROFIT_SHARING_RETURN_ORDERS("/v3/ecommerce/profitsharing/returnorders"),
    /**
     * 电商收付通-分账接口-完结分账
     */
    PROFIT_SHARING_FINISH_ORDER("/v3/ecommerce/profitsharing/finish-order"),
    /**
     * 添加分账接收方
     */
    PROFIT_SHARING_RECEIVERS_ADD("/v3/ecommerce/profitsharing/receivers/add"),
    /**
     * 删除分账接收方
     */
    PROFIT_SHARING_RECEIVERS_DELETE("/v3/ecommerce/profitsharing/receivers/delete"),
    /**
     * 退款接口-退款申请
     */
    DOMESTIC_REFUNDS("/v3/refund/domestic/refunds"),
    /**
     * 电商收付通-退款接口-退款申请
     */
    E_COMMERCE_REFUNDS("/v3/ecommerce/refunds/apply"),
    /**
     * 电商收付通-退款接口-通过微信支付退款单号查询退款
     */
    QUERY_REFUND("/v3/ecommerce/refunds/id/%s"),
    /**
     * 电商收付通-退款接口-通过商户退款单号查询退款
     */
    QUERY_REFUNDS_BY_REFUND_NO("/v3/ecommerce/refunds/out-refund-no/%s"),
    /**
     * 电商收付通-余额查询接口
     */
    QUERY_BALANCE("/v3/ecommerce/fund/balance/%s"),
    /**
     * 查询二级商户账户日终余额
     */
    QUERY_END_DAY_BALANCE("/v3/ecommerce/fund/enddaybalance/%s"),
    /**
     * 查询电商平台账户实时余额
     */
    QUERY_MERCHANT_BALANCE("/v3/merchant/fund/balance/%s"),
    /**
     * 查询电商平台账户日终余额
     */
    QUERY_MERCHANT_END_DAY_BALANCE("/v3/merchant/fund/dayendbalance/%s"),
    /**
     * 电商收付通-提现接口-账户余额提现
     */
    WITHDRAW("/v3/ecommerce/fund/withdraw"),
    /**
     * 电商收付通-提现接口-提现状态查询
     */
    QUERY_WITHDRAW("/v3/ecommerce/fund/withdraw/%s"),
    /**
     * 电商收付通-提现接口-商户提现单号查询
     */
    QUERY_WITHDRAW_BY_OUT_REQUEST_NO("/v3/ecommerce/fund/withdraw/out-request-no/%s"),
    /**
     * 电商收付通-提现接口-电商平台提现
     */
    MERCHANT_WITHDRAW("/v3/merchant/fund/withdraw"),
    /**
     * 电商收付通-提现接口-微信支付提现单号查询
     */
    QUERY_MERCHANT_WITHDRAW("/v3/ecommerce/fund/withdraw/%s"),
    /**`
     * 电商收付通-提现接口-商户提现单号查询
     */
    QUERY_MERCHANT_WITHDRAW_BY_OUT_REQUEST_NO("/v3/merchant/fund/withdraw/out-request-no/%s"),
    /**
     * 电商收付通-提现接口-按日下载提现异常文件
     */
    WITHDRAW_BILL("/v3/merchant/fund/withdraw/bill-type/%s"),
    /**
     * 申请交易账单
     */
    TRADE_BILL("/v3/bill/tradebill"),
    /**
     * 申请资金账单
     */
    FUND_FLOW_BILL("/v3/bill/fundflowbill"),
    /**
     * 下载账单
     */
    BILL_DOWNLOAD("/v3/billdownload/file"),

    /**
     * 银行特约商户违规信息查询
     */
    GET_VIOLATION("/risk/getviolation"),
    /**
     * 事前-风险商户核查接口
     */
    QUERY_MCH_RISK("/mchrisk/querymchrisk"),
    /**
     * 事后-风险商户处理结果同步接口
     */
    SYNC_MCH_RISK_RESULT("/mchrisk/syncmchriskresult"),
    /**
     * 间联模式查询商户审核状态开放接口
     */
    BANK_QUERY_MCH_AUDIT_INFO("/mchrisk/bankquerymchauditinfo"),
    /**
     * 渠道商查询商户审核信息
     */
    CHANNEL_QUERY_MCH_AUDIT_INFO("/mchrisk/channelquerymchauditinfo"),
    /**
     * 设置风险通知回调链接
     */
    SET_MCH_RISK_CALLBACK("/mchrisk/setmchriskcallback"),

    /**
     * 向员工付款
     */
    PAY_WWS_TRANS_2_POCKET("/mmpaymkttransfers/promotion/paywwsptrans2pocket"),
    /**
     * 查询向员工付款记录
     */
    QUERY_WWS_TRANS_2_POCKET("/mmpaymkttransfers/promotion/querywwsptrans2pocket"),
    /**
     * 发放企业红包
     */
    SEND_WORK_WX_RED_PACK("/mmpaymkttransfers/sendworkwxredpack"),
    /**
     * 查询企业红包记录
     */
    QUERY_WORK_WX_RED_PACK("/mmpaymkttransfers/queryworkwxredpack"),

    /**
     * 查询/更新先享卡订单
     */
    DISCOUNT_CARD_ORDER("/v3/discount-card/orders/%s"),
    /**
     * 查询先享卡订单
     */
    DISCOUNT_CARD_ORDER_TRADE_NO("/v3/discount-card/orders/out-trade-no/%s"),


    /**
     * 服务人员注册
     */
    SMART_GUIDE_GUIDES("/v3/smartguide/guides"),
    /**
     * 服务人员分配
     */
    SMART_GUIDE_GUIDES_ASSIGN("/v3/smartguide/guides/%s/assign"),
    /**
     * 服务人员信息更新
     */
    SMART_GUIDE_GUIDES_UPDATE("/v3/smartguide/guides/%s"),

    /**
     * 报关接口-订单附加信息提交接口
     */
    CUSTOM_DECLARE_ORDER("/cgi-bin/mch/customs/customdeclareorder"),
    /**
     * 报关接口-订单附加信息查询接口
     */
    CUSTOM_DECLARE_QUERY("/cgi-bin/mch/customs/customdeclarequery"),
    /**
     * 报关接口-订单附加信息重推接口
     */
    CUSTOM_DECLARE_RE_DECLARE("/cgi-bin/mch/newcustoms/customdeclareredeclare"),

    /**
     * APP 下单 API
     */
    APP_PAY("/v3/pay/transactions/app"),
    PARTNER_APP_PAY("/v3/pay/partner/transactions/app"),
    /**
     * JS API 下单 API
     */
    JS_API_PAY("/v3/pay/transactions/jsapi"),
    PARTNER_JS_API_PAY("/v3/pay/partner/transactions/jsapi"),
    /**
     * Native 下单 API
     */
    NATIVE_PAY("/v3/pay/transactions/native"),
    PARTNER_NATIVE_PAY("/v3/pay/partner/transactions/native"),
    /**
     * H5 下单 API
     */
    H5_PAY("/v3/pay/transactions/h5"),
    PARTNER_H5_PAY("/v3/pay/partner/transactions/h5"),
    /**
     * 微信支付订单号查询
     */
    ORDER_QUERY_BY_ID("/v3/pay/transactions/id/%s"),
    PARTNER_ORDER_QUERY_BY_ID("/v3/pay/partner/transactions/id/%s"),
    /**
     * 商户订单号查询
     */
    ORDER_QUERY_BY_NO("/v3/pay/transactions/out-trade-no/%s"),
    PARTNER_ORDER_QUERY_BY_NO("/v3/pay/partner/transactions/out-trade-no/%s"),
    /**
     * 关闭订单
     */
    CLOSE_ORDER_BY_NO("/v3/pay/transactions/out-trade-no/%s/close"),
    PARTNER_CLOSE_ORDER_BY_NO("/v3/pay/partner/transactions/out-trade-no/%s/close"),


    /**
     * 委托营销-建立合作关系
     */
    PARTNERSHIPS_BUILD("/v3/marketing/partnerships/build"),
    /**
     * 委托营销-终止合作关系
     */
    PARTNERSHIPS_TERMINATE("/v3/marketing/partnerships/terminate"),


    /**
     * 智慧商圈-商圈积分同步
     */
    BUSINESS_CIRCLE_POINTS_NOTIFY("/v3/businesscircle/points/notify"),


    /**
     * 连锁品牌-分账-查询分账
     */
    BRAND_PROFIT_SHARING_ORDERS("/v3/brand/profitsharing/orders"),
    /**
     * 连锁品牌-分账回退-查询分账回退
     */
    BRAND_PROFIT_SHARING_RETURN_ORDERS("/v3/brand/profitsharing/returnorders"),
    /**
     * 连锁品牌-完结分账
     */
    BRAND_PROFIT_SHARING_FINISH_ORDER("/v3/brand/profitsharing/finish-order"),
    /**
     * 连锁品牌-添加分账接收方
     */
    BRAND_PROFIT_SHARING_RECEIVERS_ADD("/v3/brand/profitsharing/receivers/add"),
    /**
     * 连锁品牌-删除分账接收方
     */
    BRAND_PROFIT_SHARING_RECEIVERS_delete("/v3/brand/profitsharing/receivers/delete"),
    ;

    /**
     * 类型
     */
    private final String url;

    WechatApiEnum(String url) {
        this.url = url;
    }

    public String getUrl() {
        return url;
    }

    @Override
    public String toString() {
        return url;
    }
}
