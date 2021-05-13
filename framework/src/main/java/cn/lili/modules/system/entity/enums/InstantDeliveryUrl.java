package cn.lili.modules.system.entity.enums;


import lombok.Getter;
import lombok.Setter;

/**
 * 即时配送url接口地址
 *
 * @author pikachu
 * @date 2020/9/11 17:03
 */
public enum InstantDeliveryUrl {

    /**
     * 订单推送
     */
    DD_ADD_ORDER("/api/order/addOrder"),
    /**
     * 订单重发
     */
    DD_RE_ADD_ORDER("/api/order/reAddOrder"),
    /**
     * 订单妥投异常后，商家确认收货
     */
    DD_CONFIRM_ORDER("/api/order/confirm/goods"),
    /**
     * 店铺添加
     */
    DD_ADD_SHOP("/api/store/add"),
    /**
     * 店铺修改
     */
    DD_UPDATE_SHOP("/api/store/update"),

    /**
     * 订单详细信息
     */
    DD_QUERY_ORDER("/api/order/status/query"),

    /**
     * 订单取消
     */
    DD_CANDLE_ORDER("/api/order/formalCancel"),

    /**
     * 城市code获取
     */
    DD_CITY_CODE("/api/cityCode/list");

    /**
     * 类型
     */
    @Getter
    @Setter
    private final String url;

    InstantDeliveryUrl(String url) {
        this.url = url;
    }

}

