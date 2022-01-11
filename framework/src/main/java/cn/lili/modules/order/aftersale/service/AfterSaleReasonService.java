package cn.lili.modules.order.aftersale.service;

import cn.lili.modules.order.aftersale.entity.dos.AfterSaleReason;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 售后原因业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:37 下午
 */
public interface AfterSaleReasonService extends IService<AfterSaleReason> {

    /**
     * 获取售后原因列表
     * @param serviceType
     * @return
     */
    List<AfterSaleReason> afterSaleReasonList(String serviceType);


    /**
     * 修改售后原因
     * @param afterSaleReason 售后原因
     * @return 售后原因
     */
    AfterSaleReason editAfterSaleReason(AfterSaleReason afterSaleReason);

}