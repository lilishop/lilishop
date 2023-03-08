package cn.lili.modules.system.service;

import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.dto.LogisticsSetting;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 物流公司业务层
 *
 * @author Chopper
 * @since 2020/11/17 8:02 下午
 */
public interface LogisticsService extends IService<Logistics> {

    /**
     * 查询物流信息
     *
     * @param logisticsId 物流公司ID
     * @param logisticsNo 单号
     * @param phone       手机号
     * @return
     */
    Traces getLogisticTrack(String logisticsId, String logisticsNo, String phone);

    /**
     * 获取物流信息
     * @param logisticsId
     * @param logisticsNo
     * @param phone
     * @param from
     * @param to
     * @return
     */
    Traces getLogisticMapTrack(String logisticsId, String logisticsNo, String phone, String from, String to);

    /**
     * 打印电子面单
     * @param orderSn 订单编号
     * @param logisticsId 物流Id
     * @return
     */
    Map labelOrder(String orderSn, String logisticsId);

    /**
     * 顺丰平台下单
     * @param orderDetailVO 订单信息
     * @return 顺丰单号
     */
    String sfCreateOrder(OrderDetailVO orderDetailVO);

    /**
     * 获取已开启的物流公司列表
     *
     * @return 物流公司列表
     */
    List<Logistics> getOpenLogistics();

    /**
     * 获取物流设置
     * @return
     */
    LogisticsSetting getLogisticsSetting();
}