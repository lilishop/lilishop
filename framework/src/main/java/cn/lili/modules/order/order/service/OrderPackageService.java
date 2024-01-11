package cn.lili.modules.order.order.service;

import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.vo.OrderPackageVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 子订单业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:36 下午
 */
public interface OrderPackageService extends IService<OrderPackage> {


    /**
     * 根据订单编号获取订单包裹列表
     * @param orderSn
     * @return
     */
    List<OrderPackage> orderPackageList(String orderSn);

    /**
     * 获取指定订单编号的所有包裹
     * @param orderSn
     * @return
     */
    List<OrderPackageVO> getOrderPackageVOList(String orderSn);
}