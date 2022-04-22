package cn.lili.common.enums;

/**
 * 返回状态码
 * 第一位 1:商品；2:用户；3:交易,4:促销,5:店铺,6:页面,7:设置,8:其他
 *
 * @author Chopper
 * @since 2020/4/8 1:36 下午
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
    LIMIT_ERROR(1003, "访问过于频繁，请稍后再试"),
    ILLEGAL_REQUEST_ERROR(1004, "非法请求，请重新刷新页面操作"),
    IMAGE_FILE_EXT_ERROR(1005, "不支持图片格式"),
    FILE_TYPE_NOT_SUPPORT(1010, "不支持上传的文件类型！"),
    PLATFORM_NOT_SUPPORTED_IM(1006, "平台未开启IM"),
    STORE_NOT_SUPPORTED_IM(1007, "店铺未开启IM"),
    UNINITIALIZED_PASSWORD(1008, "非初始化密码，无法进行初始化设置"),
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
    CATEGORY_PARAMETER_NOT_EXIST(10012, "分类绑定参数组不存在"),
    CATEGORY_PARAMETER_SAVE_ERROR(10008, "分类绑定参数组添加失败"),
    CATEGORY_PARAMETER_UPDATE_ERROR(10009, "分类绑定参数组添加失败"),
    CATEGORY_DELETE_FLAG_ERROR(10010, "子类状态不能与父类不一致！"),
    CATEGORY_COMMISSION_RATE_ERROR(10011, "分类的佣金不正确！"),

    /**
     * 商品
     */
    GOODS_ERROR(11001, "商品异常，请稍后重试"),
    GOODS_NOT_EXIST(11001, "商品已下架"),
    GOODS_NAME_ERROR(11002, "商品名称不正确，名称应为2-50字符"),
    GOODS_UNDER_ERROR(11003, "商品下架失败"),
    GOODS_UPPER_ERROR(11004, "商品上架失败"),
    GOODS_AUTH_ERROR(11005, "商品审核失败"),
    POINT_GOODS_ERROR(11006, "积分商品业务异常，请稍后重试"),
    POINT_GOODS_NOT_EXIST(11020, "积分商品不存在"),
    POINT_GOODS_CATEGORY_EXIST(11021, "当前积分商品分类已存在"),
    GOODS_SKU_SN_ERROR(11007, "商品SKU货号不能为空"),
    GOODS_SKU_PRICE_ERROR(11008, "商品SKU价格不能小于等于0"),
    GOODS_SKU_COST_ERROR(11009, "商品SKU成本价不能小于等于0"),
    GOODS_SKU_WEIGHT_ERROR(11010, "商品重量不能为负数"),
    GOODS_SKU_QUANTITY_ERROR(11011, "商品库存数量不能为负数"),
    GOODS_SKU_QUANTITY_NOT_ENOUGH(11011, "商品库存不足"),
    MUST_HAVE_GOODS_SKU(11012, "规格必须要有一个！"),
    GOODS_PARAMS_ERROR(11013, "商品参数错误，刷新后重试"),
    PHYSICAL_GOODS_NEED_TEMP(11014, "实物商品需选择配送模板"),
    VIRTUAL_GOODS_NOT_NEED_TEMP(11015, "虚拟商品无需选择配送模板"),
    GOODS_NOT_EXIST_STORE(11017, "当前用户无权操作此商品"),
    GOODS_TYPE_ERROR(11016, "需选择商品类型"),

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
    SPEC_DELETE_ERROR(13003, "分类已经绑定此规格，请先解除关联"),

    /**
     * 品牌
     */
    BRAND_SAVE_ERROR(14001, "品牌添加失败"),
    BRAND_UPDATE_ERROR(14002, "品牌修改失败"),
    BRAND_DISABLE_ERROR(14003, "品牌禁用失败"),
    BRAND_DELETE_ERROR(14004, "品牌删除失败"),
    BRAND_NAME_EXIST_ERROR(20002, "品牌名称重复！"),
    BRAND_USE_DISABLE_ERROR(20003, "分类已经绑定品牌，请先解除关联"),
    BRAND_BIND_GOODS_ERROR(20005, "品牌已经绑定商品，请先解除关联"),
    BRAND_NOT_EXIST(20004, "品牌不存在"),

    /**
     * 用户
     */
    USER_EDIT_SUCCESS(20001, "用户修改成功"),
    USER_NOT_EXIST(20002, "用户不存在"),
    USER_NOT_LOGIN(20003, "用户未登录"),
    USER_AUTH_EXPIRED(20004, "用户已退出，请重新登录"),
    USER_AUTHORITY_ERROR(20005, "权限不足"),
    USER_CONNECT_LOGIN_ERROR(20006, "未找到登录信息"),
    USER_EXIST(20008, "该用户名或手机号已被注册"),
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
    USER_NOT_BINDING(20020, "未绑定用户"),
    USER_AUTO_REGISTER_ERROR(20021, "自动注册失败,请稍后重试"),
    USER_OVERDUE_CONNECT_ERROR(20022, "授权信息已过期，请重新授权/登录"),
    USER_CONNECT_BANDING_ERROR(20023, "当前联合登陆方式，已绑定其他账号，需进行解绑操作"),
    USER_CONNECT_NOT_EXIST_ERROR(20024, "暂无联合登陆信息，无法实现一键注册功能，请点击第三方登录进行授权"),
    USER_POINTS_ERROR(20024, "用户积分不足"),

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
    CART_NUM_ERROR(30010, "购买数量必须大于0"),
    CART_PINTUAN_NOT_EXIST_ERROR(30002, "拼团活动已关闭，请稍后重试"),
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
    ORDER_NOT_SUPPORT_DISTRIBUTION(31011, "购物车中包含不支持配送的商品，请重新选择收货地址，或者重新选择商品"),
    ORDER_NOT_EXIST_VALID(31041, "购物车中无有效商品，请检查购物车内商品，或者重新选择商品"),
    ORDER_CAN_NOT_CANCEL(31012, "当前订单状态不可取消"),
    ORDER_BATCH_DELIVER_ERROR(31013, "批量发货,文件读取失败"),
    ORDER_ITEM_NOT_EXIST(31014, "当前订单项不存在！"),
    POINT_NOT_ENOUGH(31015, "当前会员积分不足购买当前积分商品！"),


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
    CAN_NOT_RECHARGE_WALLET(32012, "不能使用余额进行充值"),


    /**
     * 售后
     */
    AFTER_SALES_NOT_PAY_ERROR(33001, "当前订单未支付，不能申请售后"),
    AFTER_SALES_CANCEL_ERROR(33002, "当前售后单无法取消"),
    AFTER_SALES_BAN(33003, "订单状态不允许申请售后，请联系平台或商家"),
    AFTER_SALES_DOUBLE_ERROR(33004, "售后已审核，无法重复操作"),
    AFTER_SALES_LOGISTICS_ERROR(33005, "物流公司错误，请重新选择"),
    AFTER_STATUS_ERROR(33006, "售后状态错误，请刷新页面"),
    RETURN_MONEY_OFFLINE_BANK_ERROR(33007, "当账号类型为银行转账时，银行信息不能为空"),
    AFTER_SALES_PRICE_ERROR(33004, "申请退款金额错误"),
    AFTER_GOODS_NUMBER_ERROR(33008, "申请售后商品数量错误"),

    /**
     * 投诉
     */
    COMPLAINT_ORDER_ITEM_EMPTY_ERROR(33100, "订单不存在"),
    COMPLAINT_SKU_EMPTY_ERROR(33101, "商品已下架，如需投诉请联系平台客服"),
    COMPLAINT_ERROR(33102, "投诉异常，请稍后重试"),
    COMPLAINT_NOT_EXIT(33103, "当前投诉记录不存在"),
    COMPLAINT_ARBITRATION_RESULT_ERROR(33104, "结束订单投诉时，仲裁结果不能为空"),
    COMPLAINT_APPEAL_CONTENT_ERROR(33105, "商家申诉时，申诉内容不能为空"),
    COMPLAINT_CANCEL_ERROR(33106, "申诉已完成，不需要进行取消申诉操作"),


    /**
     * 余额
     */
    WALLET_NOT_EXIT_ERROR(34000, "钱包不存在，请联系管理员"),
    WALLET_INSUFFICIENT(34001, "余额不足以支付订单，请充值!"),
    WALLET_WITHDRAWAL_INSUFFICIENT(34002, "可提现金额不足！"),
    WALLET_WITHDRAWAL_FROZEN_AMOUNT_INSUFFICIENT(34006, "冻结金额不足，无法处理提现申请请求！"),
    WALLET_ERROR_INSUFFICIENT(34003, "零钱提现失败！"),
    WALLET_REMARK_ERROR(34004, "请填写审核备注！"),
    WALLET_EXIT_ERROR(34000, "钱包已存在，无法重复创建"),
    WALLET_APPLY_ERROR(34005, "提现申请异常！"),

    /**
     * 评价
     */
    EVALUATION_DOUBLE_ERROR(35001, "无法重复提交评价"),

    /**
     * 活动
     */
    PROMOTION_GOODS_NOT_EXIT(40000, "当前促销商品不存在！"),
    PROMOTION_GOODS_QUANTITY_NOT_EXIT(40020, "当前促销商品库存不足！"),
    PROMOTION_SAME_ACTIVE_EXIST(40001, "活动时间内已存在同类活动，请选择关闭、删除当前时段的活动"),
    PROMOTION_START_TIME_ERROR(40002, "活动起始时间不能小于当前时间"),
    PROMOTION_END_TIME_ERROR(40003, "活动结束时间不能小于当前时间"),
    PROMOTION_TIME_ERROR(40004, "活动起始时间必须大于结束时间"),
    PROMOTION_TIME_NOT_EXIST(40011, "活动起始时间和活动结束时间不能为空"),
    PROMOTION_SAME_ERROR(40005, "当前时间段已存在相同活动！"),
    PROMOTION_GOODS_ERROR(40006, "请选择要参与活动的商品"),
    PROMOTION_STATUS_END(40007, "当前活动已停止"),
    PROMOTION_UPDATE_ERROR(40008, "当前活动已开始/结束，无法编辑！"),
    PROMOTION_ACTIVITY_GOODS_ERROR(40009, "当前活动已经开始无法添加商品"),
    PROMOTION_ACTIVITY_ERROR(400010, "当前促销活动不存在"),
    PROMOTION_LOG_EXIST(40011, "活动已参加，已发重复参加"),

    /**
     * 优惠券
     */
    COUPON_LIMIT_ERROR(41000, "超出领取限制"),
    COUPON_EDIT_STATUS_SUCCESS(41001, "修改状态成功！"),
    COUPON_CANCELLATION_SUCCESS(41002, "会员优惠券作废成功"),
    COUPON_EXPIRED(41003, "优惠券已使用/已过期，不能使用"),
    COUPON_EDIT_STATUS_ERROR(41004, "优惠券修改状态失败！"),
    COUPON_RECEIVE_ERROR(41005, "当前优惠券已经被领取完了，下次要早点来哦"),
    COUPON_NUM_INSUFFICIENT_ERROR(41006, "优惠券剩余领取数量不足"),
    COUPON_NOT_EXIST(41007, "当前优惠券不存在"),
    COUPON_DO_NOT_RECEIVER(41030, "当前优惠券不允许主动领取"),
    COUPON_ACTIVITY_NOT_EXIST(410022, "当前优惠券活动不存在"),
    COUPON_SAVE_ERROR(41020, "保存优惠券失败"),
    COUPON_ACTIVITY_SAVE_ERROR(41023, "保存优惠券活动失败"),
    COUPON_DELETE_ERROR(41021, "删除优惠券失败"),
    COUPON_LIMIT_NUM_LESS_THAN_0(41008, "领取限制数量不能为负数"),
    COUPON_LIMIT_GREATER_THAN_PUBLISH(41009, "领取限制数量超出发行数量"),
    COUPON_DISCOUNT_ERROR(41010, "优惠券折扣必须小于10且大于0"),
    COUPON_SCOPE_TYPE_GOODS_ERROR(41011, "当前关联范围类型为指定商品时，商品列表不能为空"),
    COUPON_SCOPE_TYPE_CATEGORY_ERROR(41012, "当前关联范围类型为部分商品分类时，范围关联的id不能为空"),
    COUPON_SCOPE_TYPE_STORE_ERROR(41013, "当前关联范围类型为部分店铺分类时，范围关联的id不能为空"),
    COUPON_SCOPE_ERROR(41014, "指定商品范围关联id无效！"),
    COUPON_MEMBER_NOT_EXIST(41015, "没有当前会员优惠券"),
    COUPON_MEMBER_STATUS_ERROR(41016, "当前会员优惠券已过期/作废无法变更状态！"),


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
    PINTUAN_EDIT_ERROR_ITS_OPEN(42019, "拼团活动已开启，无法修改拼团活动！"),
    PINTUAN_DELETE_ERROR(42010, "删除拼团活动失败"),
    PINTUAN_JOIN_ERROR(42011, "不能参与自己发起的拼团活动！"),
    PINTUAN_LIMIT_NUM_ERROR(42012, "购买数量超过拼团活动限制数量！"),
    PINTUAN_NOT_EXIST_ERROR(42013, "当前拼团活动不存在！"),
    PINTUAN_GOODS_NOT_EXIST_ERROR(42014, "当前拼团商品不存在！"),

    /**
     * 满额活动
     */
    FULL_DISCOUNT_EDIT_SUCCESS(43001, "修改满优惠活动成功"),
    FULL_DISCOUNT_EDIT_DELETE(43002, "删除满优惠活动成功"),
    FULL_DISCOUNT_MODIFY_ERROR(43003, "当前编辑的满优惠活动已经开始或者已经结束，无法修改"),
    FULL_DISCOUNT_NOT_EXIST_ERROR(43004, "当前要操作的满优惠活动不存在！"),
    FULL_DISCOUNT_WAY_ERROR(43005, "请选择一种优惠方式！"),
    FULL_DISCOUNT_GIFT_ERROR(43006, "请选择赠品！"),
    FULL_DISCOUNT_COUPON_TIME_ERROR(43007, "赠送的优惠券有效时间必须在活动时间之内"),
    FULL_DISCOUNT_MONEY_ERROR(43008, "请填写满减金额"),
    FULL_DISCOUNT_MONEY_GREATER_THAN_MINUS(43009, "满减金额不能大于优惠门槛"),
    FULL_RATE_NUM_ERROR(43010, "请填写打折数值"),

    /**
     * 直播
     */
    STODIO_GOODS_EXIST_ERROR(44001, "直播商品已存在"),
    COMMODITY_ERROR(44002, "添加直播商品失败"),

    /**
     * 秒杀
     */
    SECKILL_NOT_START_ERROR(45000, "今日没有限时抢购活动，请明天再来看看吧。"),
    SECKILL_NOT_EXIST_ERROR(45001, "当前参与的秒杀活动不存在！"),
    SECKILL_APPLY_NOT_EXIST_ERROR(45010, "当前参与的秒杀活动不存在！"),
    SECKILL_UPDATE_ERROR(45002, "当前秒杀活动活动已经开始，无法修改！"),
    SECKILL_PRICE_ERROR(45003, "活动价格不能大于商品原价"),
    SECKILL_TIME_ERROR(45004, "时刻参数异常"),
    SECKILL_DELETE_ERROR(45005, "该秒杀活动活动的状态不能删除"),
    SECKILL_OPEN_ERROR(45010, "该秒杀活动活动的状态不能删除"),
    SECKILL_CLOSE_ERROR(45006, "该秒杀活动活动的状态不能关闭"),


    /**
     * 优惠券活动
     */
    COUPON_ACTIVITY_START_TIME_ERROR(46001, "活动时间小于当前时间，不能进行编辑删除操作"),
    COUPON_ACTIVITY_MEMBER_ERROR(46002, "指定精准发券则必须指定会员，会员不可以为空"),
    COUPON_ACTIVITY_ITEM_ERROR(46003, "优惠券活动必须指定优惠券，不能为空"),
    COUPON_ACTIVITY_ITEM_MUST_NUM_ERROR(46004, "优惠券活动最多指定10个优惠券"),
    COUPON_ACTIVITY_ITEM_NUM_ERROR(46005, "赠券数量必须大于0"),

    /**
     * 其他促销
     */
    MEMBER_SIGN_REPEAT(47001, "请勿重复签到"),
    POINT_GOODS_ACTIVE_STOCK_ERROR(47002, "活动库存数量不能高于商品库存"),
    POINT_GOODS_ACTIVE_STOCK_INSUFFICIENT(47003, "积分商品库存不足"),

    /**
     * 砍价活动
     */
    KANJIA_GOODS_ACTIVE_STOCK_ERROR(48001, "活动库存数量不能高于商品库存"),
    KANJIA_GOODS_ACTIVE_PRICE_ERROR(48002, "最低购买金额不能高于商品金额"),
    KANJIA_GOODS_ACTIVE_HIGHEST_PRICE_ERROR(48003, "最高砍价金额不能为0且不能超过商品金额"),
    KANJIA_GOODS_ACTIVE_LOWEST_PRICE_ERROR(48004, "最低砍价金额不能为0且不能超过商品金额"),
    KANJIA_GOODS_ACTIVE_HIGHEST_LOWEST_PRICE_ERROR(48005, "最低砍价金额不能高于最高砍价金额"),
    KANJIA_GOODS_ACTIVE_SETTLEMENT_PRICE_ERROR(48006, "结算金额不能高于商品金额"),
    KANJIA_GOODS_DELETE_ERROR(48007, "删除砍价商品异常"),
    KANJIA_GOODS_UPDATE_ERROR(48012, "更新砍价商品异常"),
    KANJIA_ACTIVITY_NOT_FOUND_ERROR(48008, "砍价记录不存在"),
    KANJIA_ACTIVITY_LOG_MEMBER_ERROR(48009, "当前会员已经帮砍"),
    KANJIA_ACTIVITY_MEMBER_ERROR(48010, "当前会员已经发起此砍价商品活动"),
    KANJIA_ACTIVITY_NOT_PASS_ERROR(48011, "当前砍价未满足条件，不能进行购买"),
    KANJIA_NUM_BUY_ERROR(48012, "砍价商品购买数量不正确"),
    /**
     * 店铺
     */

    STORE_NOT_EXIST(50001, "此店铺不存在"),
    STORE_NAME_EXIST_ERROR(50002, "店铺名称已存在!"),
    STORE_APPLY_DOUBLE_ERROR(50003, "已有店铺，无需重复申请!"),
    STORE_NOT_OPEN(50004, "该会员未开通店铺"),
    STORE_NOT_LOGIN_ERROR(50005, "未登录店铺"),
    STORE_CLOSE_ERROR(50006, "店铺关闭，请联系管理员"),
    FREIGHT_TEMPLATE_NOT_EXIST(50010, "当前模版不存在"),

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
    ALIPAY_NOT_SETTING(70002, "支付宝支付未配置"),
    ALIPAY_EXCEPTION(70003, "支付宝支付错误，请稍后重试"),
    ALIPAY_PARAMS_EXCEPTION(70004, "支付宝参数异常"),
    LOGISTICS_NOT_SETTING(70005, "您还未配置快递查询"),
    ORDER_SETTING_ERROR(70006, "系统订单配置异常"),
    ALI_SMS_SETTING_ERROR(70007, "您还未配置阿里云短信"),
    SMS_SIGN_EXIST_ERROR(70008, "短信签名已存在"),

    /**
     * 站内信
     */
    NOTICE_NOT_EXIST(80001, "当前消息模板不存在"),
    NOTICE_ERROR(80002, "修改站内信异常，请稍后重试"),
    NOTICE_SEND_ERROR(80003, "发送站内信异常，请检查系统日志"),


    /**
     * OSS
     */
    OSS_NOT_EXIST(80101, "OSS未配置"),
    OSS_EXCEPTION_ERROR(80102, "文件上传失败，请稍后重试"),
    OSS_DELETE_ERROR(80103, "图片删除失败"),

    /**
     * 验证码
     */
    VERIFICATION_SEND_SUCCESS(80201, "短信验证码,发送成功"),
    VERIFICATION_ERROR(80202, "验证失败"),
    VERIFICATION_CODE_INVALID(80204, "验证码已失效，请重新校验"),
    VERIFICATION_SMS_CHECKED_ERROR(80210, "短信验证码错误，请重新校验"),

    /**
     * 微信相关异常
     */
    WECHAT_CONNECT_NOT_SETTING(80300, "微信联合登陆信息未配置"),
    WECHAT_PAYMENT_NOT_SETTING(80301, "微信支付信息未配置"),
    WECHAT_QRCODE_ERROR(80302, "微信二维码生成异常"),
    WECHAT_MP_MESSAGE_ERROR(80303, "微信小程序小消息订阅异常"),
    WECHAT_JSAPI_SIGN_ERROR(80304, "微信JsApi签名异常"),
    WECHAT_CERT_ERROR(80305, "证书获取失败"),
    WECHAT_MP_MESSAGE_TMPL_ERROR(80306, "未能获取到微信模版消息id"),
    WECHAT_ERROR(80307, "微信接口异常"),
    APP_VERSION_EXIST(80307, "APP版本已存在"),

    /**
     * 其他
     */
    CUSTOM_WORDS_EXIST_ERROR(90000, "当前自定义分词已存在！"),
    CUSTOM_WORDS_NOT_EXIST_ERROR(90001, "当前自定义分词不存在！"),
    CUSTOM_WORDS_SECRET_KEY_ERROR(90002, "秘钥验证失败！"),
    CONNECT_NOT_EXIST(90000, "登录方式不存在！"),
    ELASTICSEARCH_INDEX_INIT_ERROR(90003, "索引初始化失败！"),
    PURCHASE_ORDER_DEADLINE_ERROR(90004, "供求单，已超过报名截止时间"),
    INDEX_BUILDING(90005, "索引正在生成");

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
