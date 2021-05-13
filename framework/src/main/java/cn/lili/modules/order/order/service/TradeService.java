package cn.lili.modules.order.order.service;

import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 交易业务层
 *
 * @author Chopper
 * @date 2020/11/17 7:37 下午
 */
public interface TradeService extends IService<Trade> {

    /**
     * 创建交易
     *
     * @param tradeDTO 购物车视图
     * @return 交易
     */
    Trade createTrade(TradeDTO tradeDTO);

    /**
     * 获取交易详情
     *
     * @param sn 交易编号
     * @return 交易详情
     */
    Trade getBySn(String sn);

    /**
     * 整笔交易付款
     *
     * @param tradeSn      交易编号
     * @param receivableNo 第三方流水号
     * @param paymentName  支付方式
     */
    void payTrade(String tradeSn, String paymentName, String receivableNo);

}