package cn.lili.modules.order.aftersale.service;


import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import cn.lili.modules.order.aftersale.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleApplyVO;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.aftersale.entity.vo.AfterSaleVO;
import cn.lili.modules.store.entity.dto.StoreAfterSaleAddressDTO;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.List;

/**
 * 售后业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:36 下午
 */
public interface AfterSaleService extends IService<AfterSale> {

    /**
     * 分页查询售后信息
     *
     * @param saleSearchParams 查询参数
     * @return 分页售后信息
     */
    IPage<AfterSaleVO> getAfterSalePages(AfterSaleSearchParams saleSearchParams);

    /**
     * 查询导出售后信息
     *
     * @param saleSearchParams 查询参数
     * @return 分页售后信息
     */
    List<AfterSale> exportAfterSaleOrder(AfterSaleSearchParams saleSearchParams);

    /**
     * 查询售后信息
     *
     * @param sn 售后单号
     * @return 售后信息
     */
    AfterSaleVO getAfterSale(String sn);

    /**
     * 获取申请售后页面信息
     *
     * @param sn 订单编号
     * @return
     */
    AfterSaleApplyVO getAfterSaleVO(String sn);

    /**
     * 售后申请
     *
     * @param afterSaleDTO 售后对象
     * @return 售后信息
     */
    AfterSale saveAfterSale(AfterSaleDTO afterSaleDTO);

    /**
     * 商家审核售后申请
     *
     * @param afterSaleSn   售后编号
     * @param serviceStatus 状态 PASS：审核通过，REFUSE：审核未通过
     * @param remark        商家备注
     * @param actualRefundPrice 退款金额
     * @return 售后
     */
    AfterSale review(String afterSaleSn, String serviceStatus, String remark, Double actualRefundPrice);

    /**
     * 买家退货,物流填写
     *
     * @param afterSaleSn  售后服务单号
     * @param logisticsNo  物流单号
     * @param logisticsId  物流公司ID
     * @param mDeliverTime 买家退货发货时间
     * @return 售后
     */
    AfterSale buyerDelivery(String afterSaleSn, String logisticsNo, String logisticsId, Date mDeliverTime);

    /**
     * 获取买家退货物流踪迹
     *
     * @param afterSaleSn 售后服务单号
     * @return 物流踪迹
     */
    Traces deliveryTraces(String afterSaleSn);

    /**
     * 商家收货
     *
     * @param afterSaleSn   售后编号
     * @param serviceStatus 状态 PASS：审核通过，REFUSE：审核未通过
     * @param remark        商家备注
     * @return 售后服务
     */
    AfterSale storeConfirm(String afterSaleSn, String serviceStatus, String remark);

    /**
     * 平台退款-线下支付
     *
     * @param afterSaleSn 售后单号
     * @param remark      备注
     * @return 售后服务
     */
    AfterSale refund(String afterSaleSn, String remark);

    /**
     * 买家确认解决问题
     *
     * @param afterSaleSn 售后订单sn
     * @return 售后服务
     */
    AfterSale complete(String afterSaleSn);

    /**
     * 买家取消售后
     *
     * @param afterSaleSn 售后订单sn
     * @return 售后服务
     */
    AfterSale cancel(String afterSaleSn);


    /**
     * 根据售后单号获取店铺退货收货地址信息
     *
     * @param sn 售后单号
     * @return 店铺退货收件地址
     */
    StoreAfterSaleAddressDTO getStoreAfterSaleAddressDTO(String sn);

}