package cn.lili.modules.order.order.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.Receipt;
import cn.lili.modules.order.order.entity.dto.OrderReceiptDTO;
import cn.lili.modules.order.order.entity.dto.ReceiptSearchParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 发票业务层
 *
 * @author Bulbasaur
 * @since 2020/11/17 7:37 下午
 */
public interface ReceiptService extends IService<Receipt> {


    /**
     * 根据条件获取发票信息列表
     *
     * @param searchParams 发票查询参数
     * @param pageVO       分页参数
     * @return 发票信息列表
     */
    IPage<OrderReceiptDTO> getReceiptData(ReceiptSearchParams searchParams, PageVO pageVO);

    /**
     * 根据订单编号获取发票信息
     *
     * @param orderSn 订单编号
     * @return 发票
     */
    Receipt getByOrderSn(String orderSn);

    /**
     * 获取发票详情
     *
     * @param id 发票id
     * @return 发票详情
     */
    Receipt getDetail(String id);

    /**
     * 保存发票
     *
     * @param receipt 发票信息
     * @return 是否保存成功
     */
    Receipt saveReceipt(Receipt receipt);

    /**
     * 开具发票
     *
     * @param receiptId 发票id
     * @return
     */
    Receipt invoicing(String receiptId);


}