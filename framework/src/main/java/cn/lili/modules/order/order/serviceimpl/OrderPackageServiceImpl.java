package cn.lili.modules.order.order.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.order.order.entity.dos.OrderPackage;
import cn.lili.modules.order.order.entity.dos.OrderPackageItem;
import cn.lili.modules.order.order.entity.enums.FulfillmentModeEnum;
import cn.lili.modules.order.order.entity.vo.OrderPackageVO;
import cn.lili.modules.order.order.mapper.OrderPackageMapper;
import cn.lili.modules.order.order.service.OrderPackageItemService;
import cn.lili.modules.order.order.service.OrderPackageService;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.system.service.LogisticsService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Collections;
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

    @Autowired
    private OrderMapper orderMapper;

    @Override
    public List<OrderPackage> orderPackageList(String orderSn) {
        return this.list(new LambdaQueryWrapper<OrderPackage>().eq(OrderPackage::getOrderSn, orderSn));
    }

    @Override
    public List<OrderPackageVO> getOrderPackageVOList(String orderSn) {
        AuthUser currentUser = UserContext.getCurrentUser();
        if (currentUser != null) {
            Order order = orderMapper.selectOne(new LambdaQueryWrapper<Order>()
                    .select(Order::getId, Order::getMemberId, Order::getStoreId)
                    .eq(Order::getSn, orderSn));
            if (order != null) {
                if (UserEnums.MEMBER.equals(currentUser.getRole()) && !currentUser.getId().equals(order.getMemberId())) {
                    throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
                }
                if (UserEnums.STORE.equals(currentUser.getRole()) && !currentUser.getStoreId().equals(order.getStoreId())) {
                    throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
                }
            }
        }
        List<OrderPackage> orderPackages = this.orderPackageList(orderSn);
        if (orderPackages == null || orderPackages.isEmpty()) {
            return Collections.emptyList();
        }
        List<OrderPackageVO> orderPackageVOS = new ArrayList<>();
        orderPackages.forEach(orderPackage -> {
            OrderPackageVO orderPackageVO = new OrderPackageVO(orderPackage);
            List<OrderPackageItem> orderPackageItemList = orderpackageItemService.getOrderPackageItemListByPno(orderPackage.getPackageNo());
            orderPackageVO.setOrderPackageItemList(orderPackageItemList);
            if (Boolean.TRUE.equals(orderPackageVO.getLogisticsRequired())
                    && CharSequenceUtil.isNotBlank(orderPackage.getLogisticsCode())
                    && CharSequenceUtil.isNotBlank(orderPackage.getLogisticsNo())
                    && CharSequenceUtil.isNotBlank(orderPackage.getConsigneeMobile())) {
                String mobile = orderPackage.getConsigneeMobile();
                String suffix = mobile.length() >= 4 ? mobile.substring(mobile.length() - 4) : mobile;
                Traces traces = logisticsService.getLogisticTrack(orderPackage.getLogisticsCode(), orderPackage.getLogisticsNo(), suffix);
                orderPackageVO.setTraces(traces);
            }
            orderPackageVOS.add(orderPackageVO);
        });

        return orderPackageVOS;
    }
}
