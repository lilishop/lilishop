package cn.lili.modules.order.order.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.AfterSale;
import cn.lili.modules.order.order.entity.dto.AfterSaleDTO;
import cn.lili.modules.order.order.entity.vo.AfterSaleApplyVO;
import cn.lili.modules.order.order.entity.vo.AfterSaleSearchParams;
import cn.lili.modules.order.order.entity.vo.AfterSaleVO;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
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
 * @date 2020/11/17 7:36 下午
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
    AfterSaleApplyVO getAfterSaleDTO(String sn);

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
     * 获取待处理售后数量
     *
     * @param serviceType 售后类型
     * @return 待处理售后数量
     */
    Integer applyNum(String serviceType);

    /**
     * 根据售后单号获取店铺退货收货地址信息
     *
     * @param sn 售后单号
     * @return 店铺退货收件地址
     */
    StoreAfterSaleAddressDTO getStoreAfterSaleAddressDTO(String sn);

    /**
     * 获取统计的售后
     *
     * @param statisticsQueryParam 统计搜索参数
     * @param pageVO               分页
     * @return 售后分页列表
     */
    IPage<AfterSale> getStatistics(StatisticsQueryParam statisticsQueryParam, PageVO pageVO);
}