package cn.lili.common.enums;

/**
 * 返回状态码
 * 第一位 1:商品；2:用户；3:交易,4:促销,5:店铺,6:页面,7:设置,8:其他
 *
 * @author Chopper
 * @date 2020/4/8 1:36 下午
 */
public enum ResultCode {

    /**
     * 成功状态码
     */
    SUCCESS(200, "成功"),

    /**
     * 失败返回码
     */
    ERROR(400, "服务器繁忙，请稍后重试"),

    /**
     * 失败返回码
     */
    DEMO_SITE_EXCEPTION(4001, "演示站点禁止使用"),
    /**
     * 参数异常
     */
    PARAMS_ERROR(4002, "参数异常"),


    /**
     * 系统异常
     */

    WECHAT_CONNECT_NOT_EXIST(1001, "微信联合登录未配置"),
    VERIFICATION_EXIST(1002, "验证码服务异常"),
    /**
     * 分类
     */
    CATEGORY_NOT_EXIST(10001, "商品分类不存在"),
    CATEGORY_NAME_IS_EXIST(10002, "该分类名称已存在"),
    CATEGORY_PARENT_NOT_EXIST(10003, "该分类名称已存在"),
    CATEGORY_BEYOND_THREE(10004, "最多为三级分类,添加失败"),
    CATEGORY_HAS_CHILDREN(10005, "此类别下存在子类别不能删除"),
    CATEGORY_HAS_GOODS(10006, "此类别下存在商品不能删除"),
    CATEGORY_SAVE_ERROR(10007, "此类别下存在商品不能删除"),
    CATEGORY_PARAMETER_SAVE_ERROR(10008, "分类绑定参数组添加失败"),
    CATEGORY_PARAMETER_UPDATE_ERROR(10009, "分类绑定参数组添加失败"),

    /**
     * 商品
     */
    GOODS_NOT_EXIST(11001, "商品已下架"),
    GOODS_NAME_ERROR(11002, "商品名称不正确，名称应为2-50字符"),
    GOODS_UNDER_ERROR(11003, "商品下架失败"),
    GOODS_UPPER_ERROR(11004, "商品上架失败"),
    GOODS_AUTH_ERROR(11005, "商品审核失败"),
    POINT_GOODS_ERROR(11006, "积分商品业务异常，请稍后重试"),
    GOODS_SKU_SN_ERROR(11007, "商品SKU编号不能为空"),
    GOODS_SKU_PRICE_ERROR(11008, "商品SKU价格不能小于等于0"),
    GOODS_SKU_COST_ERROR(11009, "商品SKU成本价不能小于等于0"),
    GOODS_SKU_WEIGHT_ERROR(11010, "商品重量不能为负数"),
    GOODS_SKU_QUANTITY_ERROR(11011, "商品库存数量不能为负数"),

    /**
     * 参数
     */
    PARAMETER_SAVE_ERROR(12001, "参数添加失败"),

    PARAMETER_UPDATE_ERROR(12002, "参数编辑失败"),

    /**
     * 规格
     */
    SPEC_SAVE_ERROR(13001, "规格修改失败"),

    SPEC_UPDATE_ERROR(13002, "规格修改失败"),

    SPEC_DELETE_ERROR(13003, "此规格已绑定分类不允许删除"),

    /**
     * 品牌
     */
    BRAND_SAVE_ERROR(14001, "品牌添加失败"),

    BRAND_UPDATE_ERROR(14002, "品牌修改失败"),

    BRAND_DISABLE_ERROR(14003, "品牌禁用失败"),

    BRAND_DELETE_ERROR(14004, "品牌删除失败"),

    /**
     * 用户
     */
    USER_EDIT_SUCCESS(20001, "用户修改成功"),

    USER_NOT_EXIST(20002, "用户不存在"),

    USER_NOT_LOGIN(20003, "用户未登录"),

    USER_AUTH_EXPIRED(20004, "用户已退出，请重新登录"),

    USER_AUTHORITY_ERROR(20005, "权限不足"),

    USER_CONNECT_LOGIN_ERROR(20006, "未找到登录信息"),

    USER_NAME_EXIST(20007, "该用户名已被注册"),

    USER_PHONE_EXIST(20008, "该手机号已被注册"),

    USER_PHONE_NOT_EXIST(20009, "手机号不存在"),

    USER_PASSWORD_ERROR(20010, "密码不正确"),

    USER_NOT_PHONE(20011, "非当前用户的手机号"),

    USER_CONNECT_ERROR(20012, "联合第三方登录，授权信息错误"),

    USER_RECEIPT_REPEAT_ERROR(20013, "会员发票信息重复"),

    USER_RECEIPT_NOT_EXIST(20014, "会员发票信息不存在"),

    USER_EDIT_ERROR(20015, "用户修改失败"),

    USER_OLD_PASSWORD_ERROR(20016, "旧密码不正确"),

    USER_COLLECTION_EXIST(20017, "无法重复收藏"),

    USER_GRADE_IS_DEFAULT(20018, "会员等级为默认会员等级"),

    DELETE_EXIST(2001, "无法重复收藏"),

    /**
     * 权限
     */
    PERMISSION_DEPARTMENT_ROLE_ERROR(21001, "角色已绑定部门，请逐个删除"),

    PERMISSION_USER_ROLE_ERROR(21002, "角色已绑定管理员，请逐个删除"),

    PERMISSION_MENU_ROLE_ERROR(21003, "菜单已绑定角色，请先删除或编辑角色"),

    PERMISSION_DEPARTMENT_DELETE_ERROR(21004, "部门已经绑定管理员，请先删除或编辑管理员"),

    PERMISSION_BEYOND_TEN(21005, "最多可以设置10个角色"),

    /**
     * 分销
     */
    DISTRIBUTION_CLOSE(22000, "分销功能关闭"),

    DISTRIBUTION_NOT_EXIST(22001, "分销员不存在"),

    DISTRIBUTION_IS_APPLY(22002, "分销员已申请，无需重复提交"),

    DISTRIBUTION_AUDIT_ERROR(22003, "审核分销员失败"),

    DISTRIBUTION_RETREAT_ERROR(22004, "分销员清退失败"),

    DISTRIBUTION_CASH_NOT_EXIST(22005, "分销员提现记录不存在"),

    DISTRIBUTION_GOODS_DOUBLE(22006, "不能重复添加分销商品"),

    /**
     * 购物车
     */
    CART_ERROR(30001, "读取结算页的购物车异常"),

    CART_PINTUAN_NOT_EXIST_ERROR(30002, "拼团活动不存在错误"),

    CART_PINTUAN_LIMIT_ERROR(30003, "购买数量超过拼团活动限制数量"),

    SHIPPING_NOT_APPLY(30005, "购物商品不支持当前收货地址配送"),

    /**
     * 订单
     */
    ORDER_ERROR(31001, "创建订单异常，请稍后重试"),

    ORDER_NOT_EXIST(31002, "订单不存在"),

    ORDER_DELIVERED_ERROR(31003, "订单状态错误，无法进行确认收货"),

    ORDER_UPDATE_PRICE_ERROR(31004, "已支付的订单不能修改金额"),

    ORDER_LOGISTICS_ERROR(31005, "物流错误"),

    ORDER_DELIVER_ERROR(31006, "物流错误"),

    ORDER_NOT_USER(31007, "非当前会员的订单"),

    ORDER_TAKE_ERROR(31008, "当前订单无法核销"),

    MEMBER_ADDRESS_NOT_EXIST(31009, "订单无收货地址，请先配置收货地址"),

    ORDER_DELIVER_NUM_ERROR(31010, "没有待发货的订单"),

    /**
     * 支付
     */
    PAY_UN_WANTED(32000, "当前订单不需要付款，返回订单列表等待系统订单出库即可"),

    PAY_SUCCESS(32001, "支付成功"),

    PAY_INCONSISTENT_ERROR(32002, "付款金额和应付金额不一致"),

    PAY_DOUBLE_ERROR(32003, "订单已支付，不能再次进行支付"),

    PAY_CASHIER_ERROR(32004, "收银台信息获取错误"),

    PAY_ERROR(32005, "支付业务异常，请稍后重试"),

    PAY_BAN(32006, "当前订单不需要付款，请返回订单列表重新操作"),

    PAY_PARTIAL_ERROR(32007, "该订单已部分支付，请前往订单中心进行支付"),

    PAY_NOT_SUPPORT(32008, "支付暂不支持"),

    PAY_CLIENT_TYPE_ERROR(32009, "错误的客户端"),

    PAY_POINT_ENOUGH(32010, "积分不足，不能兑换"),

    PAY_NOT_EXIST_ORDER(32011, "支付订单不存在"),

    /**
     * 售后
     */
    AFTER_SALES_NOT_PAY_ERROR(33001, "当前订单未支付，不能申请售后"),

    AFTER_SALES_CANCEL_ERROR(33002, "当前售后单无法取消"),

    AFTER_SALES_BAN(33003, "订单状态不允许申请售后，请联系平台或商家"),

    AFTER_SALES_DOUBLE_ERROR(33004, "售后已审核，无法重复操作"),

    AFTER_SALES_LOGISTICS_ERROR(33005, "物流公司错误，请重新选择"),

    AFTER_STATUS_ERROR(33006, "售后状态错误，请刷新页面"),

    /**
     * 投诉
     */
    COMPLAINT_ORDER_ITEM_EMPTY_ERROR(33100, "订单不存在"),

    COMPLAINT_SKU_EMPTY_ERROR(33101, "商品已下架，如需投诉请联系平台客服"),

    COMPLAINT_ERROR(33102, "投诉异常，请稍后重试"),

    /**
     * 余额
     */
    WALLET_NOT_EXIT_ERROR(34000, "钱包不存在，请联系管理员"),

    WALLET_INSUFFICIENT(34001, "余额不足以支付订单，请充值!"),

    WALLET_WITHDRAWAL_INSUFFICIENT(34002, "可提现金额不足！"),

    WALLET_ERROR_INSUFFICIENT(34003, "零钱提现失败！"),

    WALLET_REMARK_ERROR(34004, "请填写审核备注！"),

    WALLET_APPLY_ERROR(34005, "提现申请异常！"),

    /**
     * 评价
     */
    EVALUATION_DOUBLE_ERROR(35001, "无法重复提交评价"),

    /**
     * 签到
     */
    MEMBER_SIGN_REPEAT(40001, "请勿重复签到"),

    /**
     * 优惠券
     */
    COUPON_EDIT_STATUS_SUCCESS(41001, "修改状态成功！"),

    COUPON_CANCELLATION_SUCCESS(41002, "会员优惠券作废成功"),

    COUPON_EXPIRED(41003, "优惠券已使用/已过期，不能使用"),

    COUPON_EDIT_STATUS_ERROR(41004, "优惠券修改状态失败！"),

    /**
     * 拼团
     */
    PINTUAN_MANUAL_OPEN_SUCCESS(42001, "手动开启拼团活动成功"),

    PINTUAN_MANUAL_CLOSE_SUCCESS(42002, "手动关闭拼团活动成功"),

    PINTUAN_ADD_SUCCESS(42003, "添加拼团活动成功"),

    PINTUAN_EDIT_SUCCESS(42004, "修改拼团活动成功"),

    PINTUAN_DELETE_SUCCESS(42005, "删除拼团活动成功"),

    PINTUAN_MANUAL_OPEN_ERROR(42006, "手动开启拼团活动失败"),

    PINTUAN_MANUAL_CLOSE_ERROR(42007, "手动关闭拼团活动失败"),

    PINTUAN_ADD_ERROR(42008, "添加拼团活动失败"),

    PINTUAN_EDIT_ERROR(42009, "修改拼团活动失败"),

    PINTUAN_DELETE_ERROR(42010, "删除拼团活动失败"),

    /**
     * 满额活动
     */
    FULL_DISCOUNT_EDIT_SUCCESS(43001, "修改满优惠活动成功"),

    FULL_DISCOUNT_EDIT_DELETE(43002, "删除满优惠活动成功"),

    /**
     * 店铺
     */
    STORE_NOT_EXIST(50001, "此店铺不存在"),

    STORE_NAME_EXIST_ERROR(50002, "店铺名称已存在!"),

    STORE_APPLY_DOUBLE_ERROR(50003, "已有店铺，无需重复申请!"),

    /**
     * 结算单
     */
    BILL_CHECK_ERROR(51001, "只有已出账结算单可以核对"),

    BILL_COMPLETE_ERROR(51002, "只有已审核结算单可以支付"),

    /**
     * 文章
     */
    ARTICLE_CATEGORY_NAME_EXIST(60001, "文章分类名称已存在"),

    ARTICLE_CATEGORY_PARENT_NOT_EXIST(60002, "文章分类父分类不存在"),

    ARTICLE_CATEGORY_BEYOND_TWO(60003, "最多为二级分类,操作失败"),

    ARTICLE_CATEGORY_DELETE_ERROR(60004, "该文章分类下存在子分类，不能删除"),

    ARTICLE_CATEGORY_HAS_ARTICLE(60005, "该文章分类下存在文章，不能删除"),

    ARTICLE_CATEGORY_NO_DELETION(60007, "默认文章分类不能进行删除"),

    ARTICLE_NO_DELETION(60008, "默认文章不能进行删除"),


    /**
     * 页面
     */
    PAGE_NOT_EXIST(61001, "页面不存在"),

    PAGE_OPEN_DELETE_ERROR(61002, "当前页面为开启状态，无法删除"),

    PAGE_DELETE_ERROR(61003, "当前页面为唯一页面，无法删除"),

    PAGE_RELEASE_ERROR(61004, "页面已发布，无需重复提交"),

    /**
     * 设置
     */
    SETTING_NOT_TO_SET(70001, "该参数不需要设置"),

    /**
     * 短信
     */
    SMS_SIGN_EXIST_ERROR(80001, "短信签名已存在"),

    /**
     * 站内信
     */
    NOTICE_NOT_EXIST(80101, "当前消息模板不存在"),

    NOTICE_ERROR(80102, "修改站内信异常，请稍后重试"),

    /**
     * OSS
     */
    OSS_NOT_EXIST(80201, "OSS未配置"),

    OSS_EXCEPTION(80202, "文件上传失败，请稍后重试"),

    /**
     * 验证码
     */
    VERIFICATION_SEND_SUCCESS(80301, "短信验证码,发送成功"),

    VERIFICATION_ERROR(80302, "验证失败"),

    VERIFICATION_SMS_ERROR(80303, "短信验证码错误，请重新校验"),

    VERIFICATION_SMS_EXPIRED_ERROR(80304, "验证码已失效，请重新校验"),

    /**
     * 配置错误
     */
    ALIPAY_NOT_SETTING(80401, "支付宝支付未配置"),

    ALIPAY_EXCEPTION(80402, "支付宝支付错误，请稍后重试"),

    ALIPAY_PARAMS_EXCEPTION(80403, "支付宝参数异常"),

    /**
     * 微信相关异常
     */
    WECHAT_CONNECT_NOT_SETTING(80500, "微信联合登陆信息未配置"),

    WECHAT_PAYMENT_NOT_SETTING(80501, "微信支付信息未配置"),

    WECHAT_QRCODE_ERROR(80502, "微信二维码生成异常"),

    WECHAT_MP_MESSAGE_ERROR(80503, "微信小程序小消息订阅异常"),

    APP_VERSION_EXIST(80600, "APP版本已存在");

    private final Integer code;
    private final String message;


    ResultCode(Integer code, String message) {
        this.code = code;
        this.message = message;
    }

    public Integer code() {
        return this.code;
    }

    public String message() {
        return this.message;
    }

}
