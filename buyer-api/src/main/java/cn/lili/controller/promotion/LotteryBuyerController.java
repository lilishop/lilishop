package cn.lili.controller.promotion;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.service.LotteryService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 买家免费抽奖入口，资格、次数和流水仅使用登录会员身份。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@RestController
@Tag(name = "买家端,免费抽奖")
@RequestMapping("/buyer/promotion/lottery")
public class LotteryBuyerController {
    @Autowired
    private LotteryService service;

    /**
     * 分页查询已上架且处于有效时间区间的活动，包含公开奖池，不返回库存和内部关联字段。
     *
     * @param page 分页参数
     * @return 有效活动及公开奖池分页
     */
    @Operation(summary = "分页查询抽奖活动", description = "仅返回已上架且在有效期内的活动及公开奖池")
    @GetMapping
    public ResultMessage<?> activities(PageVO page) {
        return ResultUtil.data(service.activities(page, true));
    }

    /**
     * 查询当前登录会员在指定活动下的北京时间当天免费次数。
     *
     * @param id 抽奖活动ID
     * @return 北京时间日期、已用次数及剩余次数
     * @throws ServiceException 未登录、非会员身份或活动不存在时拒绝查询
     */
    @Operation(summary = "查询今日抽奖次数", description = "身份取当前登录会员，次数按北京时间自然日统计")
    @GetMapping("/{id}/chances")
    public ResultMessage<?> chances(@Parameter(description = "抽奖活动ID", required = true) @PathVariable String id) {
        return ResultUtil.data(service.chances(id, memberId()));
    }

    /**
     * 为当前登录会员免费抽奖，次数、库存、发奖及记录共同提交，失败整体回滚。
     * 同一会员重复请求返回原结果；网络结果不明时必须复用原请求号，下架或结束后仍可确认原结果。
     *
     * @param id 抽奖活动ID
     * @param requestId 会员内唯一请求号，1至64位字母、数字或横线，不得跨活动复用
     * @return 已完成的抽奖及发奖结果，重复请求返回原结果
     * @throws ServiceException 身份、活动资格、次数或请求号校验失败，以及发奖失败时抛出
     */
    @Operation(summary = "免费抽奖", description = "以当前会员身份抽奖并原子发奖；重试须复用请求号，避免重复发放")
    @PostMapping("/{id}/draw")
    public ResultMessage<?> draw(@Parameter(description = "抽奖活动ID", required = true) @PathVariable String id,
            @Parameter(description = "会员内唯一请求号，1至64位字母、数字或横线；重试必须复用", required = true) @RequestParam String requestId) {
        return ResultUtil.data(service.draw(id, memberId(), requestId));
    }

    /**
     * 分页查询当前登录会员的已完成抽奖记录，包含未中奖，活动下架或结束后仍可查询。
     *
     * @param page 分页参数
     * @param activityId 可选抽奖活动ID，为空时查询本人全部活动的记录
     * @return 本人已完成抽奖记录分页
     * @throws ServiceException 未登录或非会员身份时拒绝查询
     */
    @Operation(summary = "分页查询本人抽奖记录", description = "仅返回当前会员的记录，包含中奖及未中奖结果")
    @GetMapping("/records")
    public ResultMessage<?> records(PageVO page,
            @Parameter(description = "抽奖活动ID，不传则查询本人全部活动记录") @RequestParam(required = false) String activityId) {
        return ResultUtil.data(service.records(page, memberId(), activityId));
    }

    /**
     * 从安全上下文获取会员身份，禁止匿名及后台账号参与。
     *
     * @return 当前登录会员ID
     * @throws ServiceException 未登录或当前身份不是会员时抛出
     */
    private String memberId() {
        var user = UserContext.getCurrentUser();
        if (user == null || user.getRole() != UserEnums.MEMBER)
            throw new ServiceException(ResultCode.USER_AUTHORITY_ERROR);
        return user.getId();
    }
}
