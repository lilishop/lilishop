package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.core.collection.ListUtil;
import cn.hutool.core.date.DateField;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.util.ObjectUtil;
import cn.hutool.core.util.StrUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.utils.SpringContextUtil;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.enums.DistributionOrderStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import cn.lili.modules.distribution.mapper.DistributionOrderMapper;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.dto.StoreFlowQueryDTO;
import cn.lili.modules.order.order.entity.enums.OrderItemAfterSaleStatusEnum;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.order.service.StoreFlowService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;


/**
 * 分销订单接口实现
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Slf4j
@Service
public class DistributionOrderServiceImpl extends ServiceImpl<DistributionOrderMapper, DistributionOrder> implements DistributionOrderService {

    /**
     * 订单
     */
    @Autowired
    private OrderService orderService;
    /**
     * 店铺流水
     */
    @Autowired
    private StoreFlowService storeFlowService;
    /**
     * 分销员
     */
    @Autowired
    private DistributionService distributionService;
    /**
     * 系统设置
     */
    @Autowired
    private SettingService settingService;

    @Override
    public IPage<DistributionOrder> getDistributionOrderPage(DistributionOrderSearchParams distributionOrderSearchParams) {
        return this.page(PageUtil.initPage(distributionOrderSearchParams), distributionOrderSearchParams.queryWrapper());

    }

    /**
     * 1.查看订单是否为分销订单
     * 2.查看店铺流水计算分销总佣金
     * 3.修改分销员的分销总金额、冻结金额
     *
     * @param orderSn 订单编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void calculationDistribution(String orderSn) {

        //根据订单编号获取订单数据
        Order order = orderService.getBySn(orderSn);

        //判断是否为分销订单，如果为分销订单则获取分销佣金
        if (order.getDistributionId() != null) {
            //根据订单编号获取有分销金额的店铺流水记录
            List<StoreFlow> storeFlowList = storeFlowService
                    .listStoreFlow(StoreFlowQueryDTO.builder().justDistribution(true).orderSn(orderSn).build());
            double rebate = 0.0;
            //循环店铺流水记录判断是否包含分销商品
            //包含分销商品则进行记录分销订单、计算分销总额
            for (StoreFlow storeFlow : storeFlowList) {
                if (storeFlow.getDistributionRebate() == null || storeFlow.getDistributionRebate() == 0) {
                    continue;
                }
                rebate = CurrencyUtil.add(rebate, storeFlow.getDistributionRebate());
                DistributionOrder distributionOrder = new DistributionOrder(storeFlow);
                distributionOrder.setDistributionId(order.getDistributionId());
                //分销员信息
                Distribution distribution = distributionService.getById(order.getDistributionId());
                distributionOrder.setDistributionName(distribution.getMemberName());

                //设置结算天数(解冻日期)
                Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTING.name());
                DistributionSetting distributionSetting = JSONUtil.toBean(setting.getSettingValue(), DistributionSetting.class);


                //添加分销订单
                this.save(distributionOrder);

                //记录会员的分销总额
                if (rebate != 0.0) {
                    distributionService.addRebate(rebate, order.getDistributionId());

                    //如果天数写0则立即进行结算
                    if (distributionSetting.getCashDay().equals(0)) {
                        DateTime dateTime = new DateTime();
                        dateTime = dateTime.offsetNew(DateField.DAY_OF_MONTH, -distributionSetting.getCashDay());
                        //防止事务失效，采用上下文获取bean
                        DistributionOrderService bean = SpringContextUtil.getBean(DistributionOrderService.class);
                        //分销订单结算
                        bean.updateRebate(dateTime, DistributionOrderStatusEnum.WAIT_BILL.name());
                    }
                }


            }
        }

    }

    /**
     * 1.获取订单判断是否为已付款的分销订单
     * 2.查看店铺流水记录分销佣金
     * 3.修改分销员的分销总金额、可提现金额
     *
     * @param orderSn 订单编号
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void cancelOrder(String orderSn) {
        //根据订单编号获取订单数据
        Order order = orderService.getBySn(orderSn);

        //判断是否为已付款的分销订单，则获取分销佣金
        if (order.getDistributionId() != null && order.getPayStatus().equals(PayStatusEnum.PAID.name())) {

            //根据订单编号获取有分销金额的店铺流水记录
            List<DistributionOrder> distributionOrderList = this.list(new LambdaQueryWrapper<DistributionOrder>()
                    .eq(DistributionOrder::getOrderSn, orderSn));

            //如果没有分销定单，则直接返回
            if (distributionOrderList.isEmpty()) {
                return;
            }
            //分销金额
            double rebate = 0.0;

            //包含分销商品则进行记录分销订单、计算分销总额
            for (DistributionOrder distributionOrder : distributionOrderList) {
                rebate = CurrencyUtil.add(rebate, distributionOrder.getRebate());
            }

            //如果包含分销商品则记录会员的分销总额
            if (rebate != 0.0) {
                distributionService.subCanRebate(CurrencyUtil.sub(0, rebate), order.getDistributionId());
            }
        }

        //修改分销订单的状态
        this.update(new LambdaUpdateWrapper<DistributionOrder>().eq(DistributionOrder::getOrderSn, orderSn)
                .set(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.CANCEL.name()));

    }

    @Override
    public void refundOrder(AfterSale afterSale) {
        //判断是否为分销订单
        StoreFlow refundStoreFlow = storeFlowService.queryOne(StoreFlowQueryDTO.builder().justDistribution(true).refundSn(afterSale.getSn()).build());
        if (refundStoreFlow != null) {
            //获取收款分销订单
            DistributionOrder distributionOrder = this.getOne(new LambdaQueryWrapper<DistributionOrder>()
                    .eq(DistributionOrder::getOrderItemSn, afterSale.getOrderItemSn()));
            //分销订单不存在，则直接返回
            if (distributionOrder == null) {
                return;
            }
            if (distributionOrder.getSellBackRebate() == null) {
                distributionOrder.setSellBackRebate(refundStoreFlow.getDistributionRebate());
            } else {
                distributionOrder.setSellBackRebate(CurrencyUtil.add(distributionOrder.getSellBackRebate(), refundStoreFlow.getDistributionRebate()));
            }

            distributionOrder.setRebate(CurrencyUtil.sub(distributionOrder.getRebate(), refundStoreFlow.getDistributionRebate()));
            if (distributionOrder.getRebate() == 0) {
                distributionOrder.setDistributionOrderStatus(DistributionOrderStatusEnum.REFUND.name());
            }
            this.updateById(distributionOrder);

//            修改分销员提成金额
            distributionService.subCanRebate(CurrencyUtil.sub(0, refundStoreFlow.getDistributionRebate()), distributionOrder.getDistributionId());
        }
    }

    @Override
    public void updateDistributionOrderStatus(List<OrderItem> orderItems) {
        if (orderItems.isEmpty()) {
            return;
        }

        //获取未完成分销订单
        List<DistributionOrder> distributionOrderList = this.list(new LambdaQueryWrapper<DistributionOrder>()
                .eq(DistributionOrder::getDistributionOrderStatus, DistributionOrderStatusEnum.NO_COMPLETED.name()));

        if (distributionOrderList.isEmpty()) {
            return;
        }

        List<DistributionOrder> list = ListUtil.list(false);

        orderItems.stream().forEach(orderItem -> {
            //订单售后状态为已失效并且投诉状态为已失效
            if (StrUtil.equals(OrderItemAfterSaleStatusEnum.EXPIRED.name(), orderItem.getAfterSaleStatus())) {


                List<DistributionOrder> collect = distributionOrderList.stream()
                        .filter(distributionOrder -> StrUtil.equals(distributionOrder.getOrderItemSn(), orderItem.getSn()))
                        .map((distributionOrder) -> {
                            distributionOrder.setDistributionOrderStatus(DistributionOrderStatusEnum.WAIT_BILL.name());
                            distributionOrder.setSettleCycle(new Date());
                            return distributionOrder;
                        })
                        .collect(Collectors.toList());

                list.addAll(collect);
            }

        });

        if (!list.isEmpty()) {
            //批量修改分销订单结算状态
            this.updateBatchById(list);
        }
    }

    @Override
    public void updateRebate(DateTime dateTime, String distributionOrderStatus) {
        //结算时间延后五分钟
        dateTime = dateTime.offsetNew(DateField.MINUTE, 5);
        //获取待结算订单
        List<DistributionOrder> distributionOrderList = this.list(new LambdaQueryWrapper<DistributionOrder>()
                .eq(DistributionOrder::getDistributionOrderStatus, distributionOrderStatus)
                .isNotNull(DistributionOrder::getSettleCycle)
                .le(DistributionOrder::getSettleCycle, dateTime));
        //校验待结算订单
        if (ObjectUtil.isNotNull(distributionOrderList) && distributionOrderList.size() > 0) {
            //结算分销人员信息列表
            List<Distribution> distributionUpdateList = new ArrayList<>();
            //获取分销员信息
            List<Distribution> distributionList = distributionService.list(new LambdaQueryWrapper<Distribution>()
                    .eq(Distribution::getDistributionStatus, DistributionStatusEnum.PASS.name()));
            //根据销人员获取对应分销订单
            Map<String, List<DistributionOrder>> distributionOrderList1 = distributionOrderList.stream()
                    .collect(Collectors.groupingBy(DistributionOrder::getDistributionId));

            //校验分销订单不为空
            if (ObjectUtil.isNotNull(distributionOrderList1) && distributionOrderList1.size() > 0) {
                //遍历分销订单map
                distributionOrderList1.forEach((key, value) -> {
                    //计算分销结算金额
                    distributionUpdateList.add(checkDistribution(key, value, distributionList));
                });
            }

            //校验分销信息列表不为空
            if (ObjectUtil.isNotNull(distributionUpdateList) && !distributionUpdateList.isEmpty()) {
                //修改分销员收益
                distributionService.updateBatchById(distributionUpdateList);
                distributionOrderList.stream().forEach(distributionOrder -> {
                    //修改分销订单状态为待提现
                    distributionOrder.setDistributionOrderStatus(DistributionOrderStatusEnum.WAIT_CASH.name());
                });
            }

            //修改分销订单状态
            this.updateBatchById(distributionOrderList);
        }


    }


    /**
     * 计算分销结算金额
     *
     * @param distributionId   分销ID
     * @param list             分销订单
     * @param distributionList 分销列表
     * @return
     */
    public Distribution checkDistribution(String
                                                  distributionId, List<DistributionOrder> list, List<Distribution> distributionList) {
        //获取所有待结算订单分销人员信息
        Distribution distribution = distributionList.parallelStream().filter(a -> StrUtil.equals(a.getId(), distributionId)).collect(Collectors.toList()).get(0);

        //获取分销订单总金额
        double rebate = list.stream().mapToDouble(DistributionOrder::getRebate).sum();

        //检验单分销人员冻结金额为负数时.扣除负数冻结金额后再结算
        if (distribution.getCommissionFrozen() < 0) {
            rebate = CurrencyUtil.add(distribution.getCommissionFrozen() == null ? 0.0 : distribution.getCommissionFrozen(), rebate);
        }
        //结算订单总金额+分销可提现金额
        Double canRebate = CurrencyUtil.add(rebate, distribution.getCanRebate() == null ? 0.0 : distribution.getCanRebate());
        //结算金额小于0
        if (canRebate < 0) {
            //结算订单总金额+分销可提现金额
            distribution.setCanRebate(0.0);
            //冻结金额
            distribution.setCommissionFrozen(canRebate);
        } else {
            //结算订单总金额+分销可提现金额
            distribution.setCanRebate(canRebate);
            //冻结金额
            distribution.setCommissionFrozen(0.0);
        }

        return distribution;
    }
}