package cn.lili.modules.order.order.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;

/**
 * 商家订单流水业务层
 *
 * @author Chopper
 * @date 2020/11/17 7:37 下午
 */
public interface StoreFlowService extends IService<StoreFlow> {

    /**
     * 支付订单
     *
     * @param orderSn 订单编号
     */
    void payOrder(String orderSn);

    /**
     * 订单退款
     *
     * @param afterSale 售后单
     */
    void refundOrder(AfterSale afterSale);

    /**
     * 获取商家流水
     *
     * @param storeId      商家ID
     * @param type         收入、退款
     * @param distribution 是否查看分销相关数据
     * @param pageVO       分页
     * @param startTime    开始时间
     * @param endTime      结束时间
     * @return
     */
    IPage<StoreFlow> getStoreFlow(String storeId, String type, boolean distribution, PageVO pageVO, Date startTime, Date endTime);

}