package cn.lili.modules.procurement.service;

import cn.lili.modules.procurement.entity.dos.ProcurementOrder;
import cn.lili.modules.procurement.entity.dto.AuditActionDTO;
import cn.lili.modules.procurement.entity.dto.ProcurementOrderCreateDTO;
import cn.lili.modules.procurement.entity.params.ProcurementOrderSearchParams;
import cn.lili.modules.procurement.entity.vos.ProcurementOrderVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Map;

/**
 * 采购单业务接口
 * 定义采购单的生命周期操作与查询
 * @author Bulbasaur
 * @since 2025-12-18
 */
public interface ProcurementOrderService extends IService<ProcurementOrder> {
    /**
     * 创建采购单
     * @param dto 采购单创建参数
     * @return 创建后的采购单VO
     */
    ProcurementOrderVO create(ProcurementOrderCreateDTO dto);
    /**
     * 提交采购单
     * @param id 采购单ID
     * @return 是否添加成功
     */
    boolean submit(String id);
    /**
     * 审批采购单
     * @param id 采购单ID
     * @param action 审批操作参数
     * @return 是否审批成功
     */
    boolean audit(String id, AuditActionDTO action);
    /**
     * 关闭采购单
     * @param id 采购单ID
     * @return 是否关闭成功
     */
    boolean close(String id);
    /**
     * 查询采购单详情
     * @param id 采购单ID
     * @return 采购单详情VO
     */
    ProcurementOrderVO getDetail(String id);
    /**
     * 分页查询采购单
     * @param params 分页查询参数
     * @return 采购单分页数据
     */
    IPage<ProcurementOrder> page(ProcurementOrderSearchParams params);

    /**
     * 按状态统计采购单数量（不含状态筛选条件）
     * @param params 查询参数
     * @return 状态 -> 数量
     */
    Map<String, Long> statusCount(ProcurementOrderSearchParams params);
}
