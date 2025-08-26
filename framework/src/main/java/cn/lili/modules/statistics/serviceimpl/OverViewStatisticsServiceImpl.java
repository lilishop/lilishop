package cn.lili.modules.statistics.serviceimpl;

import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.order.cart.entity.enums.DeliveryMethodEnum;
import cn.lili.modules.payment.entity.enums.PaymentMethodEnum;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.vo.BusinessCompositionDataVO;
import cn.lili.modules.statistics.entity.vo.OrderOverviewVO;
import cn.lili.modules.statistics.entity.vo.OverViewDataVO;
import cn.lili.modules.statistics.entity.vo.SourceDataVO;
import cn.lili.modules.statistics.service.OrderStatisticsService;
import cn.lili.modules.statistics.service.OverViewStatisticsService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.modules.wallet.service.RechargeService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author Bulbasaur
 * @since 2025/08/25 7:07 下午
 *
 */
@Service
public class OverViewStatisticsServiceImpl implements OverViewStatisticsService {

    @Autowired
    private RechargeService rechargeService;
    @Autowired
    private OrderStatisticsService orderStatisticsService;
    @Override
    public OverViewDataVO getOverViewDataVO(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);

        OverViewDataVO overViewDataVO = new OverViewDataVO();
        //营业收入不含数值储值金额.订单扣除退款折扣后金额（不含储值充值）
        overViewDataVO.setIncomeNoStoreValue(orderStatisticsService.getPayOrderPrice(dates,null,null));
        //优惠金额.订单的优惠金额（扣除退款，不含储值充值）
        overViewDataVO.setDiscount(orderStatisticsService.getDiscountPrice( dates));
        //营业额.营业额=营业收入+优惠金额，订单扣除退款折扣前金额（不含储值充值）
        overViewDataVO.setTurnover(CurrencyUtil.add(overViewDataVO.getIncomeNoStoreValue(), overViewDataVO.getDiscount()));
        //支付订单数.按客户支付完成时间统计的成功付款的订单数（不含退款、储值充值）
        overViewDataVO.setPayOrderNum(orderStatisticsService.getPayOrderNum(dates));
        //新增充值金额.按客户支付完成时间统计的客户储值充值本金金额
        overViewDataVO.setRecharge(rechargeService.getRecharge(dates,null));
        //使用充值金额.使用储值支付本金金额：统计时间范围内，支付和退款成功的订单中，使用储值支付且扣除退款的金额
        overViewDataVO.setRechargeUse(orderStatisticsService.orderNum(PaymentMethodEnum.WALLET.name(), dates));

        return overViewDataVO;
    }

    @Override
    public List<SourceDataVO> getSourceDataVOList(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        List<SourceDataVO> sourceDataVOList=new ArrayList<>();
        //微信
        SourceDataVO sourceDataVO=new SourceDataVO();
        sourceDataVO.setPayType(PaymentMethodEnum.WECHAT.paymentName());

        sourceDataVO.setIncome(orderStatisticsService.getPayOrderPrice(dates,PaymentMethodEnum.WECHAT,null));
        sourceDataVO.setRecharge(rechargeService.getRecharge(dates,PaymentMethodEnum.WECHAT));
        sourceDataVO.setTotal(CurrencyUtil.add(sourceDataVO.getIncome(),sourceDataVO.getRecharge()));
        sourceDataVOList.add(sourceDataVO);

        //支付宝
        SourceDataVO zhifubao=new SourceDataVO();
        zhifubao.setPayType(PaymentMethodEnum.ALIPAY.paymentName());
        zhifubao.setIncome(orderStatisticsService.getPayOrderPrice(dates,PaymentMethodEnum.ALIPAY,null));
        zhifubao.setRecharge(rechargeService.getRecharge(dates,PaymentMethodEnum.ALIPAY));
        zhifubao.setTotal(CurrencyUtil.add(zhifubao.getIncome(),zhifubao.getRecharge()));
        sourceDataVOList.add(zhifubao);

        //线下支付
        SourceDataVO bankTransfer=new SourceDataVO();
        bankTransfer.setPayType(PaymentMethodEnum.BANK_TRANSFER.paymentName());
        bankTransfer.setIncome(orderStatisticsService.getPayOrderPrice(dates,PaymentMethodEnum.BANK_TRANSFER,null));
        bankTransfer.setRecharge(rechargeService.getRecharge(dates,PaymentMethodEnum.BANK_TRANSFER));
        bankTransfer.setTotal(CurrencyUtil.add(bankTransfer.getIncome(),bankTransfer.getRecharge()));
        sourceDataVOList.add(bankTransfer);

        //余额
        SourceDataVO wallet=new SourceDataVO();
        wallet.setPayType(PaymentMethodEnum.WALLET.paymentName());
        wallet.setIncome(orderStatisticsService.getPayOrderPrice(dates,PaymentMethodEnum.WALLET,null));
        wallet.setRecharge(rechargeService.getRecharge(dates,PaymentMethodEnum.WALLET));
        wallet.setTotal(CurrencyUtil.add(wallet.getIncome(),wallet.getRecharge()));
        sourceDataVOList.add(wallet);
        return sourceDataVOList;
    }

    @Override
    public BusinessCompositionDataVO businessCompositionDataVO(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        BusinessCompositionDataVO businessCompositionDataVO=new BusinessCompositionDataVO();
        //-----订单分类构成-----

        businessCompositionDataVO.setStoreSelf(orderStatisticsService.getPayOrderPrice(dates,null, DeliveryMethodEnum.SELF_PICK_UP));
        businessCompositionDataVO.setExpress(orderStatisticsService.getPayOrderPrice(dates,null, DeliveryMethodEnum.LOGISTICS));
        businessCompositionDataVO.setOnline(orderStatisticsService.getPayOrderPrice(dates,null, DeliveryMethodEnum.VIRTUAL));

        //-----营业收入-----
        //商品销售
        businessCompositionDataVO.setIncome(orderStatisticsService.getGoodsPrice(dates));
        //运费
        businessCompositionDataVO.setFreight(orderStatisticsService.getFreight(dates));
        //商品返现（分销返佣）
        businessCompositionDataVO.setIncomeBack(orderStatisticsService.getDistribution(dates));
        //商品销售+费用构成
        businessCompositionDataVO.setIncomeComposition(
                CurrencyUtil.sub(CurrencyUtil.add(businessCompositionDataVO.getIncome(), businessCompositionDataVO.getFreight()),
                businessCompositionDataVO.getIncomeBack()));

        //-----退款统计-----
        //退款订单笔数
        businessCompositionDataVO.setRefundOrderNum(orderStatisticsService.getRefundNum(dates));
        //退款金额
        businessCompositionDataVO.setRefund(orderStatisticsService.getRefundPrice(dates));
        //退款率
        businessCompositionDataVO.setRefundRate(orderStatisticsService.getRefundRate(dates));

        //-----消费指标-----
        //支付金额
        OrderOverviewVO overview=orderStatisticsService.overview(statisticsQueryParam);
        businessCompositionDataVO.setPay(overview.getPaymentAmount());
        //折后笔单价
        businessCompositionDataVO.setPrice(CurrencyUtil.div(overview.getPaymentAmount(),overview.getPaymentOrderNum()));
        //支付人数
        businessCompositionDataVO.setPayNum(overview.getPaymentsNum());
        //折后客单价
        businessCompositionDataVO.setPriceNum(CurrencyUtil.div(overview.getPaymentAmount(),overview.getPaymentsNum()));

        return businessCompositionDataVO;
    }
}
