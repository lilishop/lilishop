package cn.lili.modules.order.aftersale.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.sensitive.SensitiveFilterScene;
import cn.lili.common.sensitive.SensitiveWordsHelper;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.order.aftersale.aop.AfterSaleLogPoint;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleApplyVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleNumVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.order.aftersale.mapper.AfterSaleMapper;
import cn.lili.modules.order.aftersale.service.AfterSaleService;
import cn.lili.modules.order.aftersale.support.AfterSaleOrderItemSyncSupport;
import cn.lili.modules.order.aftersale.support.AfterSaleQuerySupport;
import cn.lili.modules.order.aftersale.support.AfterSaleValidationSupport;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.order.trade.entity.enums.AfterSaleRefundWayEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleStatusEnum;
import cn.lili.modules.order.trade.entity.enums.AfterSaleTypeEnum;
import cn.lili.modules.payment.kit.RefundSupport;
import cn.lili.modules.promotion.service.PickupCardOrderService;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.store.service.StoreDetailService;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.support.LogisticsGatewaySupport;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.tags.AfterSaleTagsEnum;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Date;
import java.util.List;

/**
 * 售后业务层实现
 * 物流公司及退货轨迹复用独立物流网关，不依赖包含订单发货协作的物流服务。
 *
 * @author Chopper
 * @since 2020/11/17 7:38 下午
 */
@Service
public class AfterSaleServiceImpl extends ServiceImpl<AfterSaleMapper, AfterSale> implements AfterSaleService {

    @Autowired
    private PickupCardOrderService pickupCardOrderService;
    @Autowired
    private OrderService orderService;
    @Autowired
    private OrderItemService orderItemService;
    @Autowired
    private LogisticsGatewaySupport logisticsGateway;
    @Autowired
    private StoreDetailService storeDetailService;
    @Autowired
    private RefundSupport refundSupport;
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;
    @Autowired
    private AppointmentService appointmentService;
    @Autowired(required = false)
    private AfterSaleQuerySupport afterSaleQuerySupport;
    @Autowired(required = false)
    private AfterSaleValidationSupport afterSaleValidationSupport;
    @Autowired(required = false)
    private AfterSaleOrderItemSyncSupport afterSaleOrderItemSyncSupport;

    @Override
    public IPage<AfterSaleVO> getAfterSalePages(AfterSaleSearchParams saleSearchParams) {
        return baseMapper.queryByParams(PageUtil.initPage(saleSearchParams), saleSearchParams.queryWrapper());
    }

    @Override
    public AfterSaleNumVO getAfterSaleNumVO(AfterSaleSearchParams saleSearchParams) {
        return getAfterSaleQuerySupport().getAfterSaleNumVO(this.baseMapper, saleSearchParams);
    }

    @Override
    public List<AfterSale> exportAfterSaleOrder(AfterSaleSearchParams saleSearchParams) {
        return this.list(saleSearchParams.queryWrapper());
    }

    @Override
    public AfterSaleVO getAfterSale(String sn) {
        return this.baseMapper.getAfterSaleVO(sn);
    }

    @Override
    public AfterSaleApplyVO getAfterSaleVO(String sn) {
        OrderItem orderItem = orderItemService.getBySn(sn);
        Order order = OperationalJudgment.judgment(orderService.getBySn(orderItem.getOrderSn()));
        return getAfterSaleQuerySupport().buildAfterSaleApplyVO(
                orderItem, order, appointmentService, getAfterSaleValidationSupport());
    }

    @Override
    @AfterSaleLogPoint(sn = "#rvt.sn", description = "'售后申请:售后编号['+#rvt.sn+']'")
    @SystemLogPoint(description = "售后-售后申请", customerLog = "'售后申请:售后编号['+#rvt.sn+']'")
    @Transactional(rollbackFor = Exception.class)
    public AfterSale saveAfterSale(AfterSaleDTO afterSaleDTO) {
        // 过滤售后原因敏感词
        afterSaleDTO.setReason(SensitiveWordsHelper.filter(SensitiveFilterScene.REASON, afterSaleDTO.getReason()));
        // 检查当前订单是否可申请售后
        getAfterSaleValidationSupport().checkAfterSaleType(this.baseMapper, afterSaleDTO);

        // 构建并保存售后
        AfterSale afterSale = getAfterSaleValidationSupport().buildNewAfterSale(this.baseMapper, afterSaleDTO);
        this.save(afterSale);
        this.sendAfterSaleMessage(afterSale);
        getAfterSaleOrderItemSyncSupport().updateOrderItemAfterSaleStatus(afterSale);
        return afterSale;
    }

    @AfterSaleLogPoint(sn = "#afterSaleSn", description = "'审核售后:售后编号['+#afterSaleSn+']' ", serviceStatus = "#serviceStatus")
    @SystemLogPoint(description = "售后-审核售后", customerLog = "'审核售后:售后编号['+#afterSaleSn+']，'+ #serviceStatus")
    @Transactional(rollbackFor = Exception.class)
    @Override
    public AfterSale review(String afterSaleSn, String serviceStatus, String remark, Double actualRefundPrice) {
        AfterSale afterSale = OperationalJudgment.judgment(this.getBySn(afterSaleSn));
        if (!afterSale.getServiceStatus().equals(AfterSaleStatusEnum.APPLY.name())) {
            throw new ServiceException(ResultCode.AFTER_SALES_DOUBLE_ERROR);
        }
        AfterSaleStatusEnum afterSaleStatusEnum;
        if (serviceStatus.equals(AfterSaleStatusEnum.PASS.name())) {
            getAfterSaleValidationSupport().validateReviewRefundPrice(this.baseMapper, afterSale, actualRefundPrice);
            afterSale.setActualRefundPrice(actualRefundPrice);
            if (afterSale.getServiceType().equals(AfterSaleTypeEnum.RETURN_MONEY.name())) {
                if (afterSale.getRefundWay().equals(AfterSaleRefundWayEnum.ORIGINAL.name())) {
                    refundSupport.refund(afterSale);
                    afterSaleStatusEnum = AfterSaleStatusEnum.COMPLETE;
                } else {
                    afterSaleStatusEnum = AfterSaleStatusEnum.WAIT_REFUND;
                }
            } else {
                afterSaleStatusEnum = AfterSaleStatusEnum.PASS;
            }
        } else {
            afterSale.setActualRefundPrice(actualRefundPrice != null ? actualRefundPrice : 0.0);
            afterSaleStatusEnum = AfterSaleStatusEnum.REFUSE;
        }
        afterSale.setServiceStatus(afterSaleStatusEnum.name());
        afterSale.setAuditRemark(remark);

        this.updateAfterSale(afterSaleSn, afterSale);
        getAfterSaleOrderItemSyncSupport().updateOrderItemAfterSaleStatus(afterSale);
        this.sendAfterSaleMessage(afterSale);
        getAfterSaleOrderItemSyncSupport().notifyAppointmentRefundIfNeeded(afterSale);
        return afterSale;
    }

    @AfterSaleLogPoint(sn = "#afterSaleSn", description = "'买家退货,物流填写:单号['+#afterSaleSn+']，物流单号为['+#logisticsNo+']'")
    @SystemLogPoint(description = "售后-买家退货,物流填写", customerLog = "'买家退货,物流填写:单号['+#afterSaleSn+']，物流单号为['+#logisticsNo+']'")
    @Override
    public AfterSale buyerDelivery(String afterSaleSn, String logisticsNo, String logisticsId, Date mDeliverTime) {
        AfterSale afterSale = OperationalJudgment.judgment(this.getBySn(afterSaleSn));
        if (!afterSale.getServiceStatus().equals(AfterSaleStatusEnum.PASS.name())) {
            throw new ServiceException(ResultCode.AFTER_STATUS_ERROR);
        }
        Logistics logistics = logisticsGateway.getById(logisticsId);
        if (logistics == null) {
            throw new ServiceException(ResultCode.AFTER_STATUS_ERROR);
        }

        afterSale.setMLogisticsCode(logistics.getId());
        afterSale.setMLogisticsName(logistics.getName());
        afterSale.setMLogisticsNo(logisticsNo);
        afterSale.setMDeliverTime(mDeliverTime);
        afterSale.setServiceStatus(AfterSaleStatusEnum.BUYER_RETURN.name());

        this.updateAfterSale(afterSaleSn, afterSale);
        return afterSale;
    }

    @Override
    public Traces deliveryTraces(String afterSaleSn) {
        AfterSale afterSale = OperationalJudgment.judgment(this.getBySn(afterSaleSn));
        return logisticsGateway.getLogisticTrack(afterSale.getMLogisticsCode(), afterSale.getMLogisticsNo(),
                storeDetailService.getStoreDetail(afterSale.getStoreId()).getSalesConsigneeMobile());
    }

    @Override
    @AfterSaleLogPoint(sn = "#afterSaleSn", description = "#serviceStatus.equals('PASS')?" +
            "'售后-商家收货:单号['+#afterSaleSn+'],处理结果[商家收货]':'售后-商家收货:单号['+#afterSaleSn+'],处理结果[商家拒收],原因['+#remark+']'")
    @SystemLogPoint(description = "售后-商家收货", customerLog = "#serviceStatus.equals('PASS')?" +
            "'售后-商家收货:单号['+#afterSaleSn+'],处理结果[商家收货]':'售后-商家收货:单号['+#afterSaleSn+'],处理结果[商家拒收],原因['+#remark+']'")
    @Transactional(rollbackFor = Exception.class)
    public AfterSale storeConfirm(String afterSaleSn, String serviceStatus, String remark) {
        AfterSale afterSale = OperationalJudgment.judgment(this.getBySn(afterSaleSn));
        if (!afterSale.getServiceStatus().equals(AfterSaleStatusEnum.BUYER_RETURN.name())) {
            throw new ServiceException(ResultCode.AFTER_STATUS_ERROR);
        }
        AfterSaleStatusEnum afterSaleStatusEnum;
        String pass = AfterSaleStatusEnum.PASS.name();
        if (pass.equals(serviceStatus) && afterSale.getRefundWay().equals(AfterSaleRefundWayEnum.ORIGINAL.name())) {
            refundSupport.refund(afterSale);
            afterSaleStatusEnum = AfterSaleStatusEnum.COMPLETE;
        } else if (pass.equals(serviceStatus)) {
            afterSaleStatusEnum = AfterSaleStatusEnum.WAIT_REFUND;
        } else {
            afterSaleStatusEnum = AfterSaleStatusEnum.SELLER_TERMINATION;
        }
        afterSale.setServiceStatus(afterSaleStatusEnum.name());
        afterSale.setAuditRemark(remark);

        this.updateAfterSale(afterSaleSn, afterSale);
        getAfterSaleOrderItemSyncSupport().updateOrderItemAfterSaleStatus(afterSale);
        this.sendAfterSaleMessage(afterSale);
        getAfterSaleOrderItemSyncSupport().notifyAppointmentRefundIfNeeded(afterSale);
        return afterSale;
    }

    @Override
    @AfterSaleLogPoint(sn = "#afterSaleSn", description = "'售后-平台退款:单号['+#afterSaleSn+']，备注为['+#remark+']'")
    @SystemLogPoint(description = "售后-平台退款", customerLog = "'售后-平台退款:单号['+#afterSaleSn+']，备注为['+#remark+']'")
    @Transactional(rollbackFor = Exception.class)
    public AfterSale refund(String afterSaleSn, String remark) {
        AfterSale afterSale = OperationalJudgment.judgment(this.getOne(new LambdaQueryWrapper<AfterSale>()
                .eq(AfterSale::getSn, afterSaleSn).last("FOR UPDATE")));
        if (!AfterSaleStatusEnum.WAIT_REFUND.name().equals(afterSale.getServiceStatus())) {
            throw new ServiceException(ResultCode.AFTER_STATUS_ERROR);
        }
        refundSupport.refund(afterSale);
        afterSale.setServiceStatus(AfterSaleStatusEnum.COMPLETE.name());
        this.updateAfterSale(afterSaleSn, afterSale);
        this.sendAfterSaleMessage(afterSale);
        getAfterSaleOrderItemSyncSupport().notifyAppointmentRefundIfNeeded(afterSale);
        return afterSale;
    }

    @Override
    @AfterSaleLogPoint(sn = "#afterSaleSn", description = "'售后-买家确认解决:单号['+#afterSaleSn+']'")
    @SystemLogPoint(description = "售后-买家确认解决", customerLog = "'售后-买家确认解决:单号['+#afterSaleSn+']'")
    public AfterSale complete(String afterSaleSn) {
        AfterSale afterSale = this.getBySn(afterSaleSn);
        afterSale.setServiceStatus(AfterSaleStatusEnum.COMPLETE.name());
        this.updateAfterSale(afterSaleSn, afterSale);
        return afterSale;
    }

    @Override
    @AfterSaleLogPoint(sn = "#afterSaleSn", description = "'售后-买家取消:单号['+#afterSaleSn+']'")
    @SystemLogPoint(description = "售后-取消售后", customerLog = "'售后-买家取消:单号['+#afterSaleSn+']'")
    @Transactional(rollbackFor = Exception.class)
    public AfterSale cancel(String afterSaleSn) {
        AfterSale afterSale = OperationalJudgment.judgment(this.getBySn(afterSaleSn));
        if (afterSale.getServiceStatus().equals(AfterSaleStatusEnum.APPLY.name())
                || afterSale.getServiceStatus().equals(AfterSaleStatusEnum.PASS.name())) {
            afterSale.setServiceStatus(AfterSaleStatusEnum.BUYER_CANCEL.name());
            this.updateAfterSale(afterSaleSn, afterSale);
            getAfterSaleOrderItemSyncSupport().updateOrderItemAfterSaleStatus(afterSale);
            return afterSale;
        }
        throw new ServiceException(ResultCode.AFTER_SALES_CANCEL_ERROR);
    }

    @Override
    public StoreAfterSaleAddressDTO getStoreAfterSaleAddressDTO(String sn) {
        return storeDetailService.getStoreAfterSaleAddressDTO(OperationalJudgment.judgment(this.getBySn(sn)).getStoreId());
    }

    private AfterSale getBySn(String sn) {
        QueryWrapper<AfterSale> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("sn", sn);
        return this.getOne(queryWrapper);
    }

    private void updateAfterSale(String afterSaleSn, AfterSale afterSale) {
        LambdaUpdateWrapper<AfterSale> queryWrapper = Wrappers.lambdaUpdate();
        queryWrapper.eq(AfterSale::getSn, afterSaleSn);
        this.update(afterSale, queryWrapper);
    }

    /**
     * 发送售后消息
     *
     * @param afterSale 售后对象
     */
    @Transactional(rollbackFor = Exception.class)
    public void sendAfterSaleMessage(AfterSale afterSale) {
        applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("发送售后单状态变更MQ消息", rocketmqCustomProperties.getAfterSaleTopic(),
                AfterSaleTagsEnum.AFTER_SALE_STATUS_CHANGE.name(), JSONUtil.toJsonStr(afterSale)));
    }

    private AfterSaleQuerySupport getAfterSaleQuerySupport() {
        if (this.afterSaleQuerySupport == null) {
            this.afterSaleQuerySupport = new AfterSaleQuerySupport();
        }
        return this.afterSaleQuerySupport;
    }

    private AfterSaleValidationSupport getAfterSaleValidationSupport() {
        if (this.afterSaleValidationSupport == null) {
            this.afterSaleValidationSupport = new AfterSaleValidationSupport(
                    orderService, orderItemService, appointmentService, pickupCardOrderService);
        }
        return this.afterSaleValidationSupport;
    }

    private AfterSaleOrderItemSyncSupport getAfterSaleOrderItemSyncSupport() {
        if (this.afterSaleOrderItemSyncSupport == null) {
            this.afterSaleOrderItemSyncSupport = new AfterSaleOrderItemSyncSupport(
                    orderItemService, orderService, appointmentService);
        }
        return this.afterSaleOrderItemSyncSupport;
    }
}
