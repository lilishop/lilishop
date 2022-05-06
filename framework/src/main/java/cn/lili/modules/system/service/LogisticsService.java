package cn.lili.modules.system.service;

import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.vo.Traces;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

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
     * @param customerName 手机号后四位
     * @return
     */
    Traces getLogistic(String logisticsId, String logisticsNo, String customerName);

    /**
     * 获取已开启的物流公司列表
     *
     * @return 物流公司列表
     */
    List<Logistics> getOpenLogistics();
}