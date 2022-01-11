package cn.lili.modules.order.order.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dos.StoreFlow;
import cn.lili.modules.order.order.entity.dto.StoreFlowQueryDTO;
import cn.lili.modules.store.entity.vos.StoreFlowPayDownloadVO;
import cn.lili.modules.store.entity.vos.StoreFlowRefundDownloadVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 商家订单流水业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:37 下午
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
     * @param storeFlowQueryDTO 查询参数
     * @return 返回分页
     */
    IPage<StoreFlow> getStoreFlow(StoreFlowQueryDTO storeFlowQueryDTO);

    /**
     * 根据参数查询一条数据
     *
     * @param storeFlowQueryDTO 查询参数
     * @return 返回分页
     */
    StoreFlow queryOne(StoreFlowQueryDTO storeFlowQueryDTO);

    /**
     * 获取结算单地入账流水
     *
     * @param storeFlowQueryDTO 查询条件
     * @return 入账流水
     */
    List<StoreFlowPayDownloadVO> getStoreFlowPayDownloadVO(StoreFlowQueryDTO storeFlowQueryDTO);

    /**
     * 获取结算单的退款流水
     *
     * @param storeFlowQueryDTO 查询条件
     * @return 退款流水
     */
    List<StoreFlowRefundDownloadVO> getStoreFlowRefundDownloadVO(StoreFlowQueryDTO storeFlowQueryDTO);


    /**
     * 根据结算单ID获取商家流水
     *
     * @param pageVO 分页
     * @param id     结算单ID
     * @param type   类型
     * @return 商家流水
     */
    IPage<StoreFlow> getStoreFlow(String id, String type, PageVO pageVO);

    /**
     * 根据结算单ID获取商家流水
     *
     * @param pageVO 分页
     * @param id     结算单ID
     * @return 商家流水
     */
    IPage<StoreFlow> getDistributionFlow(String id, PageVO pageVO);


    /**
     * 获取店铺流水
     *
     * @param storeFlowQueryDTO 店铺流水查询参数
     * @return 商家流水集合
     */
    List<StoreFlow> listStoreFlow(StoreFlowQueryDTO storeFlowQueryDTO);
}