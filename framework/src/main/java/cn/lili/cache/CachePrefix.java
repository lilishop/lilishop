package cn.lili.cache;

import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.security.enums.UserEnums;

/**
 * 缓存前缀
 *
 * @author pikachu
 * @version 4.1
 * @since 1.0
 * 2018/3/19
 */
public enum CachePrefix {


    /**
     * nonce
     */
    NONCE,

    /**
     * 在线人数
     */
    ONLINE_NUM,


    /**
     * 会员分布数据
     */
    MEMBER_DISTRIBUTION,

    /**
     * 在线会员统计
     */
    ONLINE_MEMBER,

    /**
     * token 信息
     */
    ACCESS_TOKEN,
    /**
     * token 信息
     */
    REFRESH_TOKEN,
    /**
     * 联合登录响应
     */
    CONNECT_RESULT,
    /**
     * 微信联合登陆 session key
     */
    SESSION_KEY,
    /**
     * 权限
     */
    PERMISSION_LIST,
    /**
     * 部门id列表
     */
    DEPARTMENT_IDS,

    /**
     * 用户错误登录限制
     */
    LOGIN_TIME_LIMIT,
    /**
     * 系统设置
     */
    SETTING,

    /**
     * 验证码滑块源
     */
    VERIFICATION,

    /**
     * 验证码滑块源
     */
    VERIFICATION_IMAGE,

    /**
     * 快递平台
     */
    EXPRESS,

    /**
     * 图片验证码
     */
    CAPTCHA,

    /**
     * 商品
     */
    GOODS,

    /**
     * 商品SKU
     */
    GOODS_SKU,

    /**
     * 运费模板脚本
     */
    SHIP_SCRIPT,

    /**
     * 商品sku
     */
    SKU,

    /**
     * sku库存
     */
    SKU_STOCK,

    /**
     * 促销商品sku库存
     */
    PROMOTION_GOODS_STOCK,

    /**
     * 商品库存
     */
    GOODS_STOCK,

    /**
     * 商品分类 树状结构
     */
    CATEGORY,
    /**
     * 商品分类 集合
     */
    CATEGORY_ARRAY,
    /**
     * 浏览次数
     */
    VISIT_COUNT,
    /**
     * 存储方案
     */
    UPLOADER,
    /**
     * 地区
     */
    REGION,

    /**
     * 短信网关
     */
    SPlATFORM,
    /**
     * 短信验证码前缀
     */
    _CODE_PREFIX,
    /**
     * smtp
     */
    SMTP,
    /**
     * 系统设置
     */
    SETTINGS,
    /**
     * 电子面单
     */
    WAYBILL,
    /**
     * 短信验证码
     */
    SMS_CODE,
    /**
     * 邮箱验证码
     */
    EMAIL_CODE,
    /**
     * 管理员角色权限对照表
     */
    ADMIN_URL_ROLE,

    /**
     * 店铺管理员角色权限对照表
     */
    STORE_URL_ROLE,

    /**
     * 手机验证标识
     */
    MOBILE_VALIDATE,

    /**
     * 邮箱验证标识
     */
    EMAIL_VALIDATE,

    /**
     * 店铺运费模版列表
     */
    SHIP_TEMPLATE,

    /**
     * 店铺中某个运费模版
     */
    SHIP_TEMPLATE_ONE,

    //================促销=================
    /**
     * 促销活动
     */
    PROMOTION,
    /**
     * 促销活动
     */
    PROMOTION_GOODS,

    /*** 单品立减 */
    STORE_ID_MINUS,

    /*** 第二件半价 */
    STORE_ID_HALF_PRICE,

    /*** 满优惠 */
    STORE_ID_FULL_DISCOUNT,

    /**
     * 秒杀活动活动缓存key前缀
     */
    STORE_ID_SECKILL,

    /**
     * 团购活动缓存key前缀
     */
    STORE_ID_GROUP_BUY,

    /**
     * 积分商品缓存key前缀
     */
    STORE_ID_EXCHANGE,


    //================交易=================

    /**
     * 购物车原始数据
     */
    CART_ORIGIN_DATA_PREFIX,

    /**
     * 立即购买原始数据
     */
    BUY_NOW_ORIGIN_DATA_PREFIX,

    /**
     * 交易原始数据
     */
    TRADE_ORIGIN_DATA_PREFIX,

    /**
     * 立即购买sku
     */
    CART_SKU_PREFIX,

    /**
     * 购物车视图
     */
    CART_MEMBER_ID_PREFIX,

    /**
     * 购物车，用户选择的促销信息
     */
    CART_PROMOTION_PREFIX,


    /**
     * 交易_交易价格的前缀
     */
    PRICE_SESSION_ID_PREFIX,

    /**
     * 交易_交易单
     */
    TRADE_SESSION_ID_PREFIX,


    /**
     * 结算参数
     */
    CHECKOUT_PARAM_ID_PREFIX,

    /**
     * 交易单号前缀
     */
    TRADE_SN_CACHE_PREFIX,

    /**
     * 订单编号前缀
     */
    ORDER_SN_CACHE_PREFIX,
    /**
     * 订单编号标记
     */
    ORDER_SN_SIGN_CACHE_PREFIX,
    /**
     * 订单编号前缀
     */
    PAY_LOG_SN_CACHE_PREFIX,

    /**
     * 合同编号
     */
    CONTRACT_SN_CACHE_PREFIX,


    /**
     * 零钱
     */
    SMALL_CHANGE_CACHE_PREFIX,

    /**
     * 售后服务单号前缀
     */
    AFTER_SALE_SERVICE_PREFIX,

    /**
     * 交易
     */
    TRADE,

    /**
     * 站点导航栏
     */
    SITE_NAVIGATION,

    /**
     * 支付参数
     */
    PAYMENT_CONFIG,

    /**
     * 流程控制
     */
    FLOW,

    /**
     * 热门搜索
     */
    HOT_WORD,

    /**
     * 会员积分
     */
    MEMBER_POINT,

    /**
     * 会员积分
     */
    POINT_ORDER,


    /**
     * 微博登录
     */
    WEIBO_STATE,
    /**
     * 微博登录
     */
    QQ_STATE,
    /**
     * 微博登录
     */
    GITHUB_STATE,
    /**
     * 验证码key
     */
    VERIFICATION_KEY,
    /**
     * 验证码验证结果
     */
    VERIFICATION_RESULT,
    /**
     * 微信CGItoken
     */
    WECHAT_CGI_ACCESS_TOKEN,
    /**
     * 微信JSApitoken
     */
    WECHAT_JS_API_TOKEN,
    /**
     * 微信会话信息
     */
    WECHAT_SESSION_PARAMS,
    /**
     * 第三方用户权限
     */
    ALIPAY_CONFIG,
    /**
     * 微信支付配置
     */
    WECHAT_PAY_CONFIG,
    /**
     * 微信支付平台证书配置
     */
    WECHAT_PLAT_FORM_CERT,
    /**
     * 第三方用户权限
     */
    CONNECT_AUTH,
    /**
     * 平台PageView 统计
     */
    PV,
    /**
     * 平台UserView 统计
     */
    UV,
    /**
     * 平台 商品PV 统计
     */
    GOODS_PV,
    /**
     * 平台 商品UV 统计
     */
    GOODS_UV,
    /**
     * 店铺PageView 统计
     */
    STORE_PV,
    /**
     * 店铺UserView 统计
     */
    STORE_UV,
    /**
     * 店铺 商品PV 统计
     */
    STORE_GOODS_PV,
    /**
     * 店铺 商品UV 统计
     */
    STORE_GOODS_UV,
    /**
     * 分销员
     */
    DISTRIBUTION,

    /**
     * 找回手机
     */
    FIND_MOBILE,
    /**
     * 文章分类
     */
    ARTICLE_CATEGORY,
    /**
     * 文章
     */
    ARTICLE_CACHE,

    /**
     * 初始化索引
     */
    INIT_INDEX_PROCESS,

    /**
     * 初始化索引标示
     */
    INIT_INDEX_FLAG,

    /**
     * 店铺
     */
    STORE,
    /**
     * 店铺分类
     */
    STORE_CATEGORY,
    /**
     * 用户菜单
     * <p>
     * 这个缓存并非永久缓存，而是300秒缓存，也就是说修改用户关联的部门，关联的角色，
     * 部门关联的角色，角色关联的菜单等等，最多需要5分钟才能生效
     */
    USER_MENU,
    /**
     * 订单暂时缓存
     */
    ORDER,
    /**
     * 敏感词
     */
    SENSITIVE;


    public static String removePrefix(String str) {
        return str.substring(str.lastIndexOf("}_") + 2);
    }

    /**
     * 通用获取缓存key值
     *
     * @return 缓存key值
     */
    public String getPrefix() {
        return "{" + this.name() + "}_";
    }

    /**
     * 通用获取缓存key值
     *
     * @param typeEnum 促销枚举
     * @return 缓存key值
     */
    public String getPrefix(PromotionTypeEnum typeEnum) {
        return "{" + this.name() + "_" + typeEnum.name() + "}_";
    }

    /**
     * 获取缓存key值 + 用户端
     * 例如：三端都有用户体系，需要分别登录，如果用户名一致，则redis中的权限可能会冲突出错
     *
     * @param user 角色
     * @return 缓存key值 + 用户端
     */
    public String getPrefix(UserEnums user) {
        return "{" + this.name() + "_" + user.name() + "}_";
    }
}
