package cn.lili.modules.system.support;

import cn.lili.modules.logistics.LogisticsPluginFactory;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import cn.lili.modules.system.entity.dos.Logistics;
import cn.lili.modules.system.entity.vo.Traces;
import cn.lili.modules.system.mapper.LogisticsMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * 物流基础查询与供应商调用，供物流服务和订单履约共用，不依赖订单发货业务服务。
 * 保留原有插件选择、异常处理及调用顺序，不在此推进订单状态或新增重试。
 *
 * @author Bulbasaur
 * @since 2026-11-13
 */
@Slf4j
@Component
public class LogisticsGatewaySupport {

    private final LogisticsMapper mapper;
    private final LogisticsPluginFactory pluginFactory;

    /**
     * 注入物流公司存储及现有供应商插件工厂。
     *
     * @param mapper 物流公司存储
     * @param pluginFactory 按系统配置选择供应商的工厂
     */
    public LogisticsGatewaySupport(LogisticsMapper mapper, LogisticsPluginFactory pluginFactory) {
        this.mapper = mapper;
        this.pluginFactory = pluginFactory;
    }

    /**
     * 按主键读取物流公司，不额外限定启用状态，沿用逻辑删除规则。
     *
     * @param id 物流公司主键
     * @return 物流公司，不存在时为null
     */
    public Logistics getById(String id) {
        return mapper.selectById(id);
    }

    /**
     * 按公司编码读取物流公司，多条匹配时保留原有抛错行为。
     *
     * @param code 物流公司编码
     * @return 物流公司，不存在时为null
     */
    public Logistics getByCode(String code) {
        return mapper.selectOne(new LambdaQueryWrapper<Logistics>().eq(Logistics::getCode, code));
    }

    /**
     * 读取未逻辑删除的全部物流公司，保留批量发货原有范围，不额外筛选启用状态。
     *
     * @return 物流公司列表
     */
    public List<Logistics> list() {
        return mapper.selectList(null);
    }

    /**
     * 使用当前配置的供应商查询物流轨迹，异常时沿用原有日志并返回null，不新增重试。
     *
     * @param logisticsId 物流公司主键
     * @param logisticsNo 物流单号
     * @param phone 收件人手机号，供供应商验证
     * @return 轨迹信息，查询失败时为null
     */
    public Traces getLogisticTrack(String logisticsId, String logisticsNo, String phone) {
        try {
            return pluginFactory.filePlugin().pollQuery(getById(logisticsId), logisticsNo, phone);
        } catch (Exception e) {
            log.error("获取物流公司错误", e);
        }
        return null;
    }

    /**
     * 使用当前配置的供应商查询地图轨迹，异常时沿用原有日志并返回null，不新增重试。
     *
     * @param logisticsId 物流公司主键
     * @param logisticsNo 物流单号
     * @param phone 收件人手机号
     * @param from 寄件地址
     * @param to 收件地址
     * @return 地图轨迹信息，查询失败时为null
     */
    public Traces getLogisticMapTrack(String logisticsId, String logisticsNo, String phone, String from, String to) {
        try {
            return pluginFactory.filePlugin().pollMapTrack(getById(logisticsId), logisticsNo, phone, from, to);
        } catch (Exception e) {
            log.error("获取物流公司错误", e);
        }
        return null;
    }

    /**
     * 沿用现有顺丰寄件入口调用配置的供应商，异常继续向上传播，不修改本地订单或新增重试。
     * 外部寄件结果不由数据库事务自动回滚，调用方继续负责后续发货处理。
     *
     * @param orderDetailVO 经原业务入口校验、组装的订单详情
     * @return 供应商返回的物流单号
     */
    public String sfCreateOrder(OrderDetailVO orderDetailVO) {
        return pluginFactory.filePlugin().createOrder(orderDetailVO);
    }
}
