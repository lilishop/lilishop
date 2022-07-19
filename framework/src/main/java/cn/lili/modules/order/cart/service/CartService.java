package cn.lili.modules.order.cart.service;


import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.cart.entity.enums.CartTypeEnum;
import cn.lili.modules.order.cart.entity.vo.TradeParams;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.order.entity.vo.ReceiptVO;

/**
 * 购物车业务层
 *
 * @author Chopper
 * @since 2020-03-23 12:29 下午
 */
public interface CartService {

    /**
     * 获取整笔交易
     *
     * @param checkedWay 购物车类型
     * @return 购物车视图
     */
    TradeDTO readDTO(CartTypeEnum checkedWay);

    /**
     * 获取整个交易中勾选的购物车和商品
     *
     * @param way 获取方式
     * @return 交易信息
     */
    TradeDTO getCheckedTradeDTO(CartTypeEnum way);

    /**
     * 获取可使用的优惠券数量
     *
     * @param checkedWay 购物车类型
     * @return 可使用的优惠券数量
     */
    Long getCanUseCoupon(CartTypeEnum checkedWay);

    /**
     * 获取整个交易中勾选的购物车和商品
     *
     * @return 交易信息
     */
    TradeDTO getAllTradeDTO();

    /**
     * 购物车加入一个商品
     *
     * @param skuId    要写入的skuId
     * @param num      要加入购物车的数量
     * @param cartType 购物车类型
     * @param cover    是否覆盖购物车的数量，如果为否则累加，否则直接覆盖
     */
    void add(String skuId, Integer num, String cartType, Boolean cover);


    /**
     * 更新选中状态
     *
     * @param skuId   要写入的skuId
     * @param checked 是否选中
     */
    void checked(String skuId, boolean checked);


    /**
     * 更新某个店铺的所有商品的选中状态
     *
     * @param storeId 店铺Id
     * @param checked 是否选中
     */
    void checkedStore(String storeId, boolean checked);


    /**
     * 更新全部的选中状态
     *
     * @param checked 是否选中
     */
    void checkedAll(boolean checked);


    /**
     * 批量删除
     *
     * @param skuIds 要写入的skuIds
     */
    void delete(String[] skuIds);

    /**
     * 清空购物车
     */
    void clean();

    /**
     * 重新写入
     *
     * @param tradeDTO 购物车构建器最终要构建的成品
     */
    void resetTradeDTO(TradeDTO tradeDTO);


    /**
     * 选择收货地址
     *
     * @param shippingAddressId 收货地址id
     * @param way               购物车类型
     */
    void shippingAddress(String shippingAddressId, String way);

    /**
     * 选择发票
     *
     * @param receiptVO 发票信息
     * @param way       购物车类型
     */
    void shippingReceipt(ReceiptVO receiptVO, String way);


    /**
     * 选择配送方式
     *
     * @param storeId        店铺id
     * @param deliveryMethod 配送方式
     * @param way            购物车类型
     */
    void shippingMethod(String storeId, String deliveryMethod, String way);

    /**
     * 获取购物车商品数量
     *
     * @param checked 是否选择
     * @return 购物车商品数量
     */
    Long getCartNum(Boolean checked);

    /**
     * 选择优惠券
     *
     * @param couponId 优惠券id
     * @param way      购物车类型
     * @param use      true使用 false 弃用
     */
    void selectCoupon(String couponId, String way, boolean use);

    /**
     * 创建交易
     * 1.获取购物车类型，不同的购物车类型有不同的订单逻辑
     * 购物车类型：购物车、立即购买、虚拟商品、拼团、积分
     * 2.校验用户的收件人信息
     * 3.设置交易的基础参数
     * 4.交易信息存储到缓存中
     * 5.创建交易
     * 6.清除购物车选择数据
     *
     * @param tradeParams 创建交易参数
     * @return 交易信息
     */
    Trade createTrade(TradeParams tradeParams);
}
