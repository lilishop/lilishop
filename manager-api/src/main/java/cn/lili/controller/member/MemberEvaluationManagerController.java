package cn.lili.controller.member;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.EvaluationQueryParams;
import cn.lili.modules.member.entity.vo.MemberEvaluationListVO;
import cn.lili.modules.member.entity.vo.MemberEvaluationVO;
import cn.lili.modules.member.service.MemberEvaluationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.constraints.NotNull;

/**
 * 管理端,会员商品评价接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "管理端,会员商品评价接口")
@RequestMapping("/manager/member/evaluation")
public class MemberEvaluationManagerController {
    @Autowired
    private MemberEvaluationService memberEvaluationService;

    @PreventDuplicateSubmissions
    @ApiOperation(value = "通过id获取评论")
    @ApiImplicitParam(name = "id", value = "评价ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<MemberEvaluationVO> get(@PathVariable String id) {

        return ResultUtil.data(memberEvaluationService.queryById(id));
    }

    @ApiOperation(value = "获取评价分页")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<MemberEvaluationListVO>> getByPage(EvaluationQueryParams evaluationQueryParams, PageVO page) {

        return ResultUtil.data(memberEvaluationService.queryPage(evaluationQueryParams));
    }

    @PreventDuplicateSubmissions
    @ApiOperation(value = "修改评价状态")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "评价ID", required = true, paramType = "path"),
            @ApiImplicitParam(name = "status", value = "显示状态,OPEN 正常 ,CLOSE 关闭", required = true, paramType = "query")
    })
    @GetMapping(value = "/updateStatus/{id}")
    public ResultMessage<Object> updateStatus(@PathVariable String id, @NotNull String status) {
        memberEvaluationService.updateStatus(id, status);
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除评论")
    @ApiImplicitParam(name = "id", value = "评价ID", required = true, dataType = "String", paramType = "path")
    @PutMapping(value = "/delete/{id}")
    public ResultMessage<IPage<Object>> delete(@PathVariable String id) {
        memberEvaluationService.delete(id);
        return ResultUtil.success();
    }

}
