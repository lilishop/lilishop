package cn.lili.controller.manager.member;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
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

import jakarta.validation.constraints.NotNull;

/**
 * 管理端,客户商品评价接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "管理端,客户商品评价接口")
@RequestMapping("/manager/member/evaluation")
public class MemberEvaluationManagerController {
    @Autowired
    private MemberEvaluationService memberEvaluationService;

    @PreventDuplicateSubmissions
    @Operation(description = "通过id获取评论")
    @Parameter(name = "id", description = "评价ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberEvaluationVO> get(@PathVariable String id) {

        return ResultUtil.data(memberEvaluationService.queryById(id));
    }

    @Operation(description = "获取评价分页")
    @Parameter(name = "evaluationQueryParams", description = "评价查询参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<MemberEvaluationListVO>> getByPage(EvaluationQueryParams evaluationQueryParams, PageVO page) {

        return ResultUtil.data(memberEvaluationService.queryPage(evaluationQueryParams));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "修改评价状态")
    @Parameter(name = "id", description = "评价ID", required = true)
    @Parameter(name = "status", description = "显示状态,OPEN 正常 ,CLOSE 关闭", required = true)
    @GetMapping("/updateStatus/{id}")
    public ResultMessage<Object> updateStatus(@PathVariable String id, @NotNull String status) {
        memberEvaluationService.updateStatus(id, status);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(description = "修改评价置顶状态")
    @Parameter(name = "id", description = "评价ID", required = true)
    @Parameter(name = "top", description = "是否置顶", required = true)
    @PutMapping("/updateTop/{id}")
    public ResultMessage<Object> updateTop(@PathVariable String id, @NotNull Boolean top) {
        memberEvaluationService.updateTop(id, top);
        return ResultUtil.success();
    }

    @Operation(description = "删除评论")
    @Parameter(name = "id", description = "评价ID", required = true)
    @PutMapping("/delete/{id}")
    public ResultMessage<IPage<Object>> delete(@PathVariable String id) {
        memberEvaluationService.delete(id);
        return ResultUtil.success();
    }

}
