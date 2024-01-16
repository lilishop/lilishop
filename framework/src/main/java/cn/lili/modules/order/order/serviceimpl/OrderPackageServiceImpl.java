package cn.lili.modules.order.order.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dos.OrderPackageItem;
import cn.lili.modules.order.order.entity.vo.OrderPackageVO;
import cn.lili.modules.order.order.mapper.OrderPackageMapper;
import cn.lili.modules.order.order.service.OrderPackageItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.service.LogisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;

/**
 * 订单包裹业务层实现
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class OrderPackageServiceImpl extends ServiceImpl<OrderPackageMapper, OrderPackage> implements OrderPackageService {

    @Autowired
    private OrderPackageItemService orderpackageItemService;

    @Autowired
    private LogisticsService logisticsService;

    @Override
    public List<OrderPackage> orderPackageList(String orderSn) {
        return this.list(new LambdaQueryWrapper<OrderPackage>().eq(OrderPackage::getOrderSn, orderSn));
    }

    @Override
    public List<OrderPackageVO> getOrderPackageVOList(String orderSn) {
        List<OrderPackage> orderPackages = this.orderPackageList(orderSn);
        if (orderPackages == null){
            throw new ServiceException(ResultCode.ORDER_PACKAGE_NOT_EXIST);
        }
        List<OrderPackageVO> orderPackageVOS = new ArrayList<>();
        orderPackages.forEach(orderPackage -> {
            OrderPackageVO orderPackageVO = new OrderPackageVO(orderPackage);
            // 获取子订单包裹详情
            List<OrderPackageItem> orderPackageItemList = orderpackageItemService.getOrderPackageItemListByPno(orderPackage.getPackageNo());
            orderPackageVO.setOrderPackageItemList(orderPackageItemList);
            String str = orderPackage.getConsigneeMobile();
            str = str.substring(str.length() - 4);
//            Traces traces = logisticsService.getLogisticTrack(orderPackage.getLogisticsCode(), orderPackage.getLogisticsNo(), str);
//            orderPackageVO.setTraces(traces);
            orderPackageVOS.add(orderPackageVO);
        });

        return orderPackageVOS;
    }
}