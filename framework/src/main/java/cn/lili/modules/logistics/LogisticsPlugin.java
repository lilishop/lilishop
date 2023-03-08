package cn.lili.modules.logistics;

import cn.lili.modules.logistics.entity.dto.LabelOrderDTO;
import cn.lili.modules.logistics.entity.enums.LogisticsEnum;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.vo.Traces;

import java.util.Map;

/**
 * 物流插件接口
 *
 * @author Bulbasaur
 * @author Bulbasaur
 * @since 2023-02-16
 */
public interface LogisticsPlugin {


    /**
     * 插件名称
     */
    LogisticsEnum pluginName();

    /**
     * 实时查询快递
     *
     * @param logistics 物流公司
     * @param expNo
     * @param phone
     * @return 物流信息
     */
    Traces pollQuery(Logistics logistics, String expNo, String phone);

    /**
     * 实时查询地图轨迹
     *
     * @param logistics 物流公司
     * @param expNo     单号
     * @param phone     收件人手机号
     * @param from      出发地信息，最小颗粒到市级，例如：广东省深圳市
     * @param to        目的地信息，最小颗粒到市级，例如：广东省深圳市
     * @return 物流信息
     */
    Traces pollMapTrack(Logistics logistics, String expNo, String phone, String from, String to);

    /**
     * 电子面单打印
     *
     * @param labelOrderDTO 电子面单DTO
     * @return
     */
    Map labelOrder(LabelOrderDTO labelOrderDTO);

    String createOrder(OrderDetailVO orderDetailVO);

}
