package cn.lili.modules.order.aftersale.service;

import cn.lili.modules.order.aftersale.entity.dos.AfterSaleLog;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 订单日志业务层
 *
 * @author Chopper
 * @since 2020/11/17 7:37 下午
 */
public interface AfterSaleLogService extends IService<AfterSaleLog> {

    /**
     * 获取售后日志
     *
     * @param sn 售后编号
     * @return 售后日志列表
     */
    List<AfterSaleLog> getAfterSaleLog(String sn);
}