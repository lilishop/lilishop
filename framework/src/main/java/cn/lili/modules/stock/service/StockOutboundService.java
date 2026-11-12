package cn.lili.modules.stock.service;

import cn.lili.modules.stock.entity.dos.StockOutbound;
import cn.lili.modules.stock.entity.dto.StockOutboundAuditDTO;
import cn.lili.modules.stock.entity.dto.StockOutboundCreateDTO;
import cn.lili.modules.stock.entity.params.StockOutboundSearchParams;
import cn.lili.modules.stock.entity.vos.StockOutboundVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 商家出库单服务接口
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public interface StockOutboundService extends IService<StockOutbound> {

    /**
     * 分页查询出库单列表
     *
     * @param params 查询参数
     * @return 分页结果
     */
    IPage<StockOutbound> page(StockOutboundSearchParams params);

    /**
     * 查询出库单详情（包含商品明细列表）
     *
     * @param id 出库单ID
     * @return 出库单详情VO
     */
    StockOutboundVO getDetail(String id);

    /**
     * 创建出库单（草稿状态）
     *
     * @param dto 创建参数
     * @return 创建后的出库单VO
     */
    StockOutboundVO createDraft(StockOutboundCreateDTO dto);

    /**
     * 编辑出库单（仅限草稿或已驳回状态）
     *
     * @param id  出库单ID
     * @param dto 编辑参数
     * @return 修改后的出库单VO
     */
    StockOutboundVO updateDraft(String id, StockOutboundCreateDTO dto);

    /**
     * 提交出库单审核（草稿/已驳回 -> 待审核，重置旧审核信息）
     *
     * @param id 出库单ID
     * @return 是否成功
     */
    boolean submit(String id);

    /**
     * 审核出库单（通过扣库存并完成 / 驳回填写原因）
     *
     * @param id  出库单ID
     * @param dto 审核参数
     * @return 是否成功
     */
    boolean audit(String id, StockOutboundAuditDTO dto);

    /**
     * 作废出库单（仅限草稿/待审核/已驳回状态）
     *
     * @param id 出库单ID
     * @return 是否成功
     */
    boolean cancel(String id);
}
