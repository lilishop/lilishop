package cn.lili.modules.system.entity.plugin.InstantDelivery;

import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.store.entity.vos.StoreDetailVO;
import cn.lili.modules.system.entity.plugin.ConfigItem;
import cn.lili.modules.system.entity.vo.InstantDeliveryResultVO;

import java.util.List;
import java.util.Map;

/**
 * 即时配送插件方法
 *
 * @author pikachu
 * @version v1.0
 * @Description:
 * @since 2020/12/01 15:58
 */
public interface InstantDeliveryPlugin {
    /**
     * 获取即时配送的插件ID
     *
     * @return
     */
    String getPluginId();

    /**
     * 获取即时配送的插件名称
     *
     * @return 插件名称
     */
    String getPluginName();

    /**
     * 即时配送是否开启
     *
     * @return 0 不开启  1 开启
     */
    Integer getOpen();

    /**
     * 获取取消原因id
     *
     * @return
     */
    Integer cancelReasonId();

    /**
     * 配置各个即时配送的参数
     *
     * @return 在页面加载的即时配送参数
     */
    List<ConfigItem> getDefaultConfigItem();

    /**
     * 同城配送新建店铺
     *
     * @param storeDetailVO 店铺信息
     * @param config        配送参数
     * @return
     */
    InstantDeliveryResultVO addStore(StoreDetailVO storeDetailVO, Map config);

    /**
     * 同城配送新建店铺
     *
     * @param storeDetailVO 店铺信息
     * @param config        配送参数
     * @return
     */
    InstantDeliveryResultVO editStore(StoreDetailVO storeDetailVO, Map config);

    /**
     * 查询订单详细
     *
     * @param orderSn 传递到达达的订单sn，目前使用的是商城订单sn传递的所有只需要传递商城sn就可以
     * @param config  配置参数
     * @return
     */
    InstantDeliveryResultVO getOrderDetail(String orderSn, Map config);

    /**
     * 妥投异常之物品返回完成
     *
     * @param orderSn 传递到达达的订单sn，目前使用的是商城订单sn传递的所有只需要传递商城sn就可以
     * @param config  配置参数
     * @return
     */
    InstantDeliveryResultVO orderConfirm(String orderSn, Map config);

    /**
     * 妥投异常之物品返回完成
     *
     * @param orderSn      传递到达达的订单sn，目前使用的是商城订单sn传递的所有只需要传递商城sn就可以
     * @param cancelReason 取消原因
     * @param config       配置参数
     * @return
     */
    InstantDeliveryResultVO orderCandle(String orderSn, String cancelReason, Map config);

    /**
     * 发送同城配送订单
     *
     * @param order         订单
     * @param memberAddress 会员地址
     * @param type          类型
     * @param config        配置
     * @param storeDetailVO 店铺详情VO
     * @return 配送订单返回
     */
    InstantDeliveryResultVO sendReOrder(Order order, StoreDetailVO storeDetailVO, MemberAddress memberAddress, Integer type, Map config);

    /**
     * 即时配送回调
     *
     * @param object 因为不同配送的返回对象不同，所以需要obj 去传递参数，在不同的插件中转换
     */
    void callBack(Object object);
}
