package cn.lili.modules.order.order.service;

import cn.lili.modules.order.order.entity.dos.OrderPackageItem;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 子订单业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:36 下午
 */
public interface OrderPackageItemService extends IService<OrderPackageItem> {


    /**
     * 根据订单编号获取订单包裹列表
     * @param orderSn 订单编号
     * @return 子订单包裹列表
     */
    List<OrderPackageItem> getOrderPackageItemListByOrderSn(String orderSn);


    /**
     * 根据包裹编号获取子包裹列表
     * @param packageNo 包裹编号
     * @return 子包裹列表
     */
    List<OrderPackageItem> getOrderPackageItemListByPno(String packageNo);
}