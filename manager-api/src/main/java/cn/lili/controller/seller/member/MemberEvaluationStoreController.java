package cn.lili.controller.seller.member;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.OperationalJudgment;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.EvaluationQueryParams;
import cn.lili.modules.member.entity.vo.MemberEvaluationListVO;
import cn.lili.modules.member.entity.vo.MemberEvaluationVO;
import cn.lili.modules.member.service.MemberEvaluationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.Objects;

/**
 * 店铺端,商品评价管理接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "店铺端,商品评价管理接口")
@RequestMapping("/store/member/evaluation")
public class MemberEvaluationStoreController {

    @Autowired
    private MemberEvaluationService memberEvaluationService;

    @Operation(summary = "分页获取客户评论列表")
    @GetMapping
    public ResultMessage<IPage<MemberEvaluationListVO>> getByPage(EvaluationQueryParams evaluationQueryParams) {
        String storeId = Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        evaluationQueryParams.setStoreId(storeId);
        return ResultUtil.data(memberEvaluationService.queryPage(evaluationQueryParams));
    }

    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "评价ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberEvaluationVO> get(@PathVariable String id) {
        return ResultUtil.data(OperationalJudgment.judgment(memberEvaluationService.queryById(id)));
    }

    @Operation(summary = "回复评价")
    @Parameter(name = "id", description = "评价ID", required = true)
    @Parameter(name = "reply", description = "回复内容", required = true)
    @Parameter(name = "replyImage", description = "回复图片")
    @PutMapping("/reply/{id}")
    public ResultMessage<MemberEvaluationVO> reply(@PathVariable String id, @RequestParam String reply, @RequestParam String replyImage) {
        OperationalJudgment.judgment(memberEvaluationService.queryById(id));
        memberEvaluationService.reply(id, reply, replyImage);
        return ResultUtil.success();
    }
}
