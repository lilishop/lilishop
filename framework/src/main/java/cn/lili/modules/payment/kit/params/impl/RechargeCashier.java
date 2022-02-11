package cn.lili.modules.payment.kit.params.impl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.payment.entity.enums.CashierEnum;
import cn.lili.modules.payment.kit.dto.PayParam;
import cn.lili.modules.payment.kit.dto.PaymentSuccessParams;
import cn.lili.modules.payment.kit.params.CashierExecute;
import cn.lili.modules.payment.kit.params.dto.CashierParam;
import cn.lili.modules.system.entity.dto.BaseSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.modules.wallet.entity.dos.Recharge;
import cn.lili.modules.wallet.service.RechargeService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * 充值信息获取
 *
 * @author Chopper
 * @since 2021-01-25 20:00
 */
@Slf4j
@Component
public class RechargeCashier implements CashierExecute {
    /**
     * 余额
     */
    @Autowired
    private RechargeService rechargeService;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;


    @Override
    public CashierEnum cashierEnum() {
        return CashierEnum.RECHARGE;
    }

    @Override
    public void paymentSuccess(PaymentSuccessParams paymentSuccessParams) {
        PayParam payParam = paymentSuccessParams.getPayParam();
        if (payParam.getOrderType().equals(CashierEnum.RECHARGE.name())) {
            rechargeService.paySuccess(payParam.getSn(), paymentSuccessParams.getReceivableNo(),paymentSuccessParams.getPaymentMethod());
            log.info("会员充值-订单号{},第三方流水：{}", payParam.getSn(), paymentSuccessParams.getReceivableNo());
        }
    }


    @Override
    public CashierParam getPaymentParams(PayParam payParam) {
        if (payParam.getOrderType().equals(CashierEnum.RECHARGE.name())) {
            //准备返回的数据
            CashierParam cashierParam = new CashierParam();
            //订单信息获取
            Recharge recharge = rechargeService.getRecharge(payParam.getSn());

            //如果订单已支付，则不能发器支付
            if (recharge.getPayStatus().equals(PayStatusEnum.PAID.name())) {
                throw new ServiceException(ResultCode.PAY_DOUBLE_ERROR);
            }


            cashierParam.setPrice(recharge.getRechargeMoney());

            try {
                BaseSetting baseSetting = JSONUtil.toBean(settingService.get(SettingEnum.BASE_SETTING.name()).getSettingValue(), BaseSetting.class);
                cashierParam.setTitle(baseSetting.getSiteName());
            } catch (Exception e) {
                cashierParam.setTitle("多用户商城，在线充值");
            }
            cashierParam.setDetail("余额充值");
            cashierParam.setCreateTime(recharge.getCreateTime());
            return cashierParam;
        }

        return null;
    }

    @Override
    public Boolean paymentResult(PayParam payParam) {
        if (payParam.getOrderType().equals(CashierEnum.RECHARGE.name())) {
            Recharge recharge = rechargeService.getRecharge(payParam.getSn());
            if (recharge != null) {
                return recharge.getPayStatus().equals(PayStatusEnum.PAID.name());
            } else {
                throw new ServiceException(ResultCode.PAY_NOT_EXIST_ORDER);
            }
        }
        return false;
    }
}
