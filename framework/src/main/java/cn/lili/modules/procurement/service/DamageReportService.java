package cn.lili.modules.procurement.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.procurement.entity.dos.DamageReport;
import cn.lili.modules.procurement.entity.dto.DamageReportCreateDTO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.Map;

/**
 * 报损单业务接口
 * 定义报损单的生命周期操作
 * @author Bulbasaur
 * @since 2025-12-18
 */
public interface DamageReportService extends IService<DamageReport> {
    /**
     * 创建报损单
     * @param dto 报损单创建参数
     * @return 创建后的报损单
     */
    DamageReport create(DamageReportCreateDTO dto);
    /**
     * 提交报损单
     * @param id 报损单ID
     * @return 提交后的报损单
     */
    DamageReport submit(String id);
    /**
     * 审批通过报损单
     * @param id 报损单ID
     * @return 审批通过后的报损单
     */
    DamageReport approve(String id);
    /**
     * 审批拒绝报损单
     * @param id 报损单ID
     * @param remark 拒绝备注
     * @return 审批拒绝后的报损单
     */
    DamageReport reject(String id, String remark);
    /**
     * 撤销报损单
     * @param id 报损单ID
     * @return 撤销后的报损单
     */
    DamageReport cancel(String id);
    /**
     * 完成报损单
     * @param id 报损单ID
     * @return 完成后的报损单
     */
    DamageReport complete(String id);

    /**
     * 分页查询报损单。
     *
     * @param pageVO    分页参数
     * @param storeId   店铺ID（为空则不过滤）
     * @param status    状态（为空则不过滤）
     * @param startDate 开始时间（与 endDate 同时存在才生效）
     * @param endDate   结束时间（与 startDate 同时存在才生效）
     * @return 报损单分页数据
     */
    IPage<DamageReport> pageByCondition(PageVO pageVO, String storeId, String status, Date startDate, Date endDate);

    /**
     * 按状态统计报损单数量（不含状态筛选条件）
     */
    Map<String, Long> statusCount(String storeId, Date startDate, Date endDate);
}
