package cn.lili.modules.order.order.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dto.MemberAddressDTO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dto.OrderExportDTO;
import cn.lili.modules.order.order.entity.dto.OrderMessage;
import cn.lili.modules.order.order.entity.dto.OrderSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;
import java.util.List;

/**
 * 子订单业务层
 *
 * @author Chopper
 * @date 2020/11/17 7:36 下午
 */
public interface OrderService extends IService<Order> {


    /**
     * 系统取消订单
     *
     * @param orderSn 订单编号
     * @param reason  错误原因
     */
    void systemCancel(String orderSn, String reason);

    /**
     * 根据sn查询
     *
     * @param orderSn 订单编号
     * @return 订单信息
     */
    Order getBySn(String orderSn);

    /**
     * 订单查询
     *
     * @param orderSearchParams 查询参数
     * @return 简短订单分页
     */
    IPage<OrderSimpleVO> queryByParams(OrderSearchParams orderSearchParams);

    /**
     * 查询导出订单列表
     *
     * @param orderSearchParams 查询参数
     * @return 导出订单列表
     */
    List<OrderExportDTO> queryExportOrder(OrderSearchParams orderSearchParams);


    /**
     * 订单详细
     *
     * @param orderSn 订单SN
     * @return 订单详细
     */
    OrderDetailVO queryDetail(String orderSn);

    /**
     * 创建订单
     * 1.检查交易信息
     * 2.循环交易购物车列表，创建订单以及相关信息
     *
     * @param tradeDTO 交易DTO
     */
    void intoDB(TradeDTO tradeDTO);

    /**
     * 订单付款
     * 修改订单付款信息
     * 记录订单流水
     *
     * @param orderSn       订单编号
     * @param paymentMethod 支付方法
     * @param receivableNo  第三方流水
     */
    void payOrder(String orderSn, String paymentMethod, String receivableNo);

    /**
     * 订单确认成功
     *
     * @param orderSn
     */
    void afterOrderConfirm(String orderSn);

    /**
     * 取消订单
     *
     * @param orderSn 订单SN
     * @param reason  取消理由
     * @return 订单
     */
    Order cancel(String orderSn, String reason);


    /**
     * 发货信息修改
     * 日志功能内部实现
     *
     * @param orderSn          订单编号
     * @param memberAddressDTO 收货地址信息
     * @return 订单
     */
    Order updateConsignee(String orderSn, MemberAddressDTO memberAddressDTO);

    /**
     * 订单发货
     *
     * @param orderSn       订单编号
     * @param invoiceNumber 发货单号
     * @param logisticsId   物流公司
     * @return 订单
     */
    Order delivery(String orderSn, String invoiceNumber, String logisticsId);

    /**
     * 获取物流踪迹
     *
     * @param orderSn 订单编号
     * @return 物流踪迹
     */
    Traces getTraces(String orderSn);

    /**
     * 订单核验
     *
     * @param verificationCode 验证码
     * @param orderSn          订单编号
     * @return 订单
     */
    Order take(String orderSn, String verificationCode);

    /**
     * 根据核验码获取订单信息
     *
     * @param verificationCode 验证码
     * @return 订单
     */
    Order getOrderByVerificationCode(String verificationCode);

    /**
     * 订单完成
     *
     * @param orderSn 订单编号
     */
    void complete(String orderSn);

    /**
     * 通过trade 获取订单列表
     *
     * @param tradeSn 交易编号
     * @return 订单列表
     */
    List<Order> getByTradeSn(String tradeSn);

    /**
     * 发送更新订单状态的信息
     *
     * @param orderMessage 订单传输信息
     */
    void sendUpdateStatusMessage(OrderMessage orderMessage);

    /**
     * 根据订单sn逻辑删除订单
     *
     * @param sn 订单sn
     */
    void deleteOrder(String sn);

    /**
     * 获取统计的订单
     *
     * @param statisticsQueryParam
     * @param pageVO
     * @return
     */
    IPage<OrderSimpleVO> getStatistics(StatisticsQueryParam statisticsQueryParam, PageVO pageVO);


    /**
     * 开具发票
     *
     * @param sn 订单sn
     * @return
     */
    Boolean invoice(String sn);

    /**
     * 自动成团订单处理
     *
     * @param pintuanId     拼团活动id
     * @param parentOrderSn 拼团订单sn
     */
    void agglomeratePintuanOrder(String pintuanId, String parentOrderSn);

    /**
     * 获取待发货订单编号列表
     *
     * @param response
     * @param logisticsName 店铺已选择物流公司列表
     * @return 待发货订单编号列表
     */
    void getBatchDeliverList(HttpServletResponse response, List<String> logisticsName);

    /**
     * 订单批量发货
     *
     * @param files 文件
     */
    void batchDeliver(MultipartFile files);

}