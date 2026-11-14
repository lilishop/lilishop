package cn.lili.modules.order.order.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.appointment.entity.dos.AppointmentVerificationCode;
import cn.lili.modules.appointment.entity.enums.AppointmentVerificationCodeStatusEnum;
import cn.lili.modules.appointment.entity.vo.AppointmentVerificationResultVO;
import cn.lili.modules.appointment.support.AppointmentVerificationSupport;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.dto.OrderVerificationRecordSearchParams;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.order.order.entity.vo.OrderVerificationGoodsVO;
import cn.lili.modules.order.order.entity.vo.OrderVerificationPreviewVO;
import cn.lili.modules.order.order.entity.vo.OrderVerificationRecordVO;
import cn.lili.modules.order.order.mapper.OrderMapper;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * 订单核销与自提支撑组件
 * <p>
 * 负责核销码验证、自提订单与虚拟订单提货核销、核销扫码预览以及门店核销台账记录分页查询。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class OrderVerificationSupport {

    private final OrderItemService orderItemService;
    private final OrderPickupSupport orderPickupSupport;
    private final AppointmentVerificationSupport appointmentVerificationSupport;

    /**
     * 注入订单明细、预约协作和带事务及日志代理的提货组件。
     *
     * @param orderItemService 订单明细查询
     * @param appointmentVerificationSupport 预约核销组件，不经过完整预约服务
     * @param orderPickupSupport 自提及虚拟订单核销组件
     */
    public OrderVerificationSupport(OrderItemService orderItemService,
                                   AppointmentVerificationSupport appointmentVerificationSupport,
                                   OrderPickupSupport orderPickupSupport) {
        this.orderItemService = orderItemService;
        this.appointmentVerificationSupport = appointmentVerificationSupport;
        this.orderPickupSupport = orderPickupSupport;
    }

    /**
     * 校验自提/虚拟订单核销资格与核销码准确性
     *
     * @param order            订单实体
     * @param verificationCode 核销码
     */
    public void checkVerificationOrder(Order order, String verificationCode) {
        orderPickupSupport.checkVerificationOrder(order, verificationCode);
    }

    /**
     * 指定订单与核销码提货核销
     *
     * @param orderSn          订单编号
     * @param verificationCode 核销码
     * @param orderService     订单服务接口
     * @return 提货完成后的订单
     */
    public Order take(String orderSn, String verificationCode, OrderService orderService) {
        return orderPickupSupport.take(orderSn, verificationCode, orderService);
    }

    /**
     * 按核销码直接提货
     *
     * @param verificationCode 核销码
     * @param orderService     订单服务接口
     * @return 提货完成后的订单
     */
    public Order take(String verificationCode, OrderService orderService) {
        String storeId = OperationalJudgment.judgment(UserContext.getCurrentUser()).getStoreId();
        Order order = orderService.getOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getVerificationCode, verificationCode)
                .eq(Order::getStoreId, storeId));
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_NOT_EXIST);
        }
        order.setOrderStatus(OrderStatusEnum.COMPLETED.name());
        orderService.complete(order.getSn());
        return order;
    }

    /**
     * 扫码确认核销
     *
     * @param code         核销码字符串
     * @param orderService 订单服务接口
     * @return 核销结果文案
     */
    public String confirmVerification(String code, OrderService orderService) {
        if (CharSequenceUtil.isBlank(code)) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        String verificationCode = code.trim();
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        String userId = UserContext.getCurrentUser().getId();
        if (appointmentVerificationSupport.hasVerificationCode(storeId, verificationCode)) {
            AppointmentVerificationResultVO result = appointmentVerificationSupport.verifyCodeByOperator(storeId, userId, verificationCode);
            if (Boolean.TRUE.equals(result.getAllVerified())) {
                return Boolean.TRUE.equals(result.getOrderCompleted())
                        ? "核销全部完成，订单已完结"
                        : "核销全部完成，工单进行中";
            }
            return "核销成功，已核销 " + result.getVerifiedCount() + "/" + result.getTotalCount() + " 次";
        }
        Order order = orderService.getOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getStoreId, storeId)
                .eq(Order::getVerificationCode, verificationCode)
                .eq(Order::getDeleteFlag, false));
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        if (!OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())
                && !OrderStatusEnum.STAY_PICKED_UP.name().equals(order.getOrderStatus())) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        orderPickupSupport.take(order.getSn(), verificationCode, orderService);
        return "核销成功";
    }

    /**
     * 核销前扫码预览待核销商品信息
     *
     * @param code         核销码
     * @param orderService 订单服务接口
     * @return 核销预览视图对象
     */
    public OrderVerificationPreviewVO previewVerification(String code, OrderService orderService) {
        if (CharSequenceUtil.isBlank(code)) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        String verificationCode = code.trim();
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        AppointmentVerificationCode appointmentCode = appointmentVerificationSupport.findVerificationCode(storeId, verificationCode);
        if (appointmentCode != null) {
            if (!AppointmentVerificationCodeStatusEnum.UNUSED.name().equals(appointmentCode.getStatus())) {
                throw new ServiceException(ResultCode.APPOINTMENT_CODE_ALREADY_USED);
            }
            OrderItem item = orderItemService.getBySn(appointmentCode.getOrderItemSn());
            OrderVerificationGoodsVO goods = toVerificationGoods(item, 1);
            if (CharSequenceUtil.isBlank(goods.getOrderItemSn())) {
                goods.setOrderItemSn(appointmentCode.getOrderItemSn());
            }
            OrderVerificationPreviewVO preview = new OrderVerificationPreviewVO();
            preview.setCode(appointmentCode.getCode());
            preview.setOrderSn(appointmentCode.getOrderSn());
            preview.setGoods(Collections.singletonList(goods));
            return preview;
        }
        Order order = orderService.getOne(new LambdaQueryWrapper<Order>()
                .eq(Order::getStoreId, storeId)
                .eq(Order::getVerificationCode, verificationCode)
                .eq(Order::getDeleteFlag, false));
        if (order == null
                || (!OrderStatusEnum.TAKE.name().equals(order.getOrderStatus())
                && !OrderStatusEnum.STAY_PICKED_UP.name().equals(order.getOrderStatus()))) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        List<OrderItem> items = orderItemService.getByOrderSn(order.getSn());
        OrderVerificationPreviewVO preview = new OrderVerificationPreviewVO();
        preview.setCode(order.getVerificationCode());
        preview.setOrderSn(order.getSn());
        preview.setGoods(items.stream()
                .map(item -> toVerificationGoods(item, item.getNum()))
                .collect(Collectors.toList()));
        return preview;
    }

    /**
     * 封装核销商品行数据
     *
     * @param item 订单项实体
     * @param num  核销数量
     * @return 预览商品行
     */
    public OrderVerificationGoodsVO toVerificationGoods(OrderItem item, Integer num) {
        OrderVerificationGoodsVO goods = new OrderVerificationGoodsVO();
        goods.setNum(num);
        if (item == null) {
            return goods;
        }
        goods.setOrderItemSn(item.getSn());
        goods.setGoodsName(item.getGoodsName());
        goods.setImage(item.getImage());
        goods.setSpecs(item.getSpecs());
        return goods;
    }

    /**
     * 分页查询门店核销台账记录
     *
     * @param params      查询参数
     * @param orderMapper 订单持久层映射器
     * @return 核销记录分页
     */
    public IPage<OrderVerificationRecordVO> verificationRecordPage(OrderVerificationRecordSearchParams params, OrderMapper orderMapper) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        if (params == null) {
            params = new OrderVerificationRecordSearchParams();
        }
        params.setGoodsName(CharSequenceUtil.trimToNull(params.getGoodsName()));
        params.setVerifierName(CharSequenceUtil.trimToNull(params.getVerifierName()));
        return orderMapper.pageVerificationRecords(PageUtil.initPageWithoutOrder(params), storeId, params);
    }

    /**
     * 根据核销码查询指定提货状态订单
     *
     * @param verificationCode 核销码
     * @param orderService     订单服务接口
     * @return 订单实体
     */
    public Order getOrderByVerificationCode(String verificationCode, OrderService orderService) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        Order order = orderService.getOne(new LambdaQueryWrapper<Order>()
                .in(Order::getOrderStatus, OrderStatusEnum.TAKE.name(), OrderStatusEnum.STAY_PICKED_UP.name())
                .eq(Order::getStoreId, storeId)
                .eq(Order::getVerificationCode, verificationCode));
        if (order == null) {
            throw new ServiceException(ResultCode.ORDER_TAKE_ERROR);
        }
        return order;
    }
}
