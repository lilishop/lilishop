package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.LotteryActivity;
import cn.lili.modules.promotion.service.LotteryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 平台抽奖草稿、发布及流水管理，使用抽奖活动独立菜单权限，发布后奖池不可编辑。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@RestController
@Tag(name = "管理端,免费抽奖")
@RequestMapping("/manager/promotion/lottery")
public class LotteryManagerController {
    @Autowired
    private LotteryService service;

    /**
     * 分页查询所有状态的抽奖活动，包含奖池配置、累计库存及已发放份数，供管理和只读详情使用。
     *
     * @param page 分页参数
     * @return 活动及完整奖池分页
     */
    @Operation(summary = "分页查询抽奖活动", description = "查询所有状态的活动及完整奖池，包含库存和已发放份数")
    @GetMapping
    public ResultMessage<?> activities(PageVO page) {
        return ResultUtil.data(service.activities(page, false));
    }

    /**
     * 创建或修改未发布的活动草稿，保存活动配置与奖池；已发布活动禁止修改。
     * 概率以万分点传入，对应正整数百分比，全部奖项合计100%，且恰好包含一个谢谢参与。
     *
     * @param input 活动及奖池配置；无ID时新建，有ID时编辑草稿
     * @return 已保存的草稿及奖池
     * @throws ServiceException 活动已发布、配置不合法或保存失败时抛出，保存失败整体回滚
     */
    @Operation(summary = "保存抽奖活动草稿", description = "无ID时新建，有ID时编辑未发布草稿；首次发布后禁止修改配置")
    @PostMapping
    public ResultMessage<?> configure(@Parameter(description = "抽奖活动及奖池配置", required = true) @RequestBody LotteryActivity input) {
        return ResultUtil.data(service.configure(input));
    }

    /**
     * 上下架抽奖活动，首次上架后冻结活动配置及奖池。
     * 下架后停止新抽奖，保留已发奖励和记录；活动结束后禁止再次上架。
     *
     * @param id      抽奖活动ID
     * @param enabled true上架、false下架
     * @return 状态更新成功结果
     * @throws ServiceException 活动不存在、已结束而请求上架、奖池或券模板无效、状态更新失败时抛出
     */
    @Operation(summary = "上下架抽奖活动", description = "首次上架冻结配置；下架保留奖励和记录，已结束活动不能上架")
    @PutMapping("/{id}/status")
    public ResultMessage<?> status(@Parameter(description = "抽奖活动ID", required = true) @PathVariable String id,
                                   @Parameter(description = "true上架、false下架", required = true) @RequestParam boolean enabled) {
        service.status(id, enabled);
        return ResultUtil.success();
    }

    /**
     * 分页查询会员已完成的抽奖记录，包含中奖及未中奖结果，支持按活动筛选。
     *
     * @param page       分页参数
     * @param activityId 可选抽奖活动ID，为空时查询全部活动记录
     * @return 已完成抽奖记录分页
     */
    @Operation(summary = "分页查询抽奖记录", description = "查询中奖及未中奖记录，可按抽奖活动筛选")
    @GetMapping("/records")
    public ResultMessage<?> records(PageVO page,
                                    @Parameter(description = "抽奖活动ID，不传则查询全部活动记录") @RequestParam(required = false) String activityId) {
        return ResultUtil.data(service.records(page, null, activityId));
    }
}
