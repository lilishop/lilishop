package cn.lili.modules.payment.service;

import cn.lili.modules.order.order.entity.vo.PaymentLog;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;

/**
 * 支付日志 业务层
 *
 * @author Chopper
 * @date 2020-12-19 09:25
 */
public interface PaymentService {

    /**
     * 支付成功通知
     *
     * @param paymentSuccessParams 支付成功回调参数
     */
    void success(PaymentSuccessParams paymentSuccessParams);


    /**
     * 平台支付成功
     *
     * @param paymentSuccessParams 支付成功回调参数
     */
    void adminPaySuccess(PaymentSuccessParams paymentSuccessParams);


    /**
     * 获取支付日志
     *
     * @param initPage
     * @param initWrapper
     * @return
     */
    IPage<PaymentLog> page(Page<PaymentLog> initPage, QueryWrapper<PaymentLog> initWrapper);
}