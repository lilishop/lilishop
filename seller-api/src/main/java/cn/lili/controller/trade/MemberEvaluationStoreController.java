package cn.lili.controller.trade;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.utils.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dto.StoreEvaluationQueryParams;
import cn.lili.modules.member.entity.vo.MemberEvaluationListVO;
import cn.lili.modules.member.entity.vo.MemberEvaluationVO;
import cn.lili.modules.member.service.MemberEvaluationService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 店铺端,商品评价管理接口
 *
 * @author Bulbasaur
 * @date 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "店铺端,商品评价管理接口")
@RequestMapping("/store/memberEvaluation")
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class MemberEvaluationStoreController {

    private final MemberEvaluationService memberEvaluationService;

    @ApiOperation(value = "分页获取会员评论列表")
    @GetMapping
    public ResultMessage<IPage<MemberEvaluationListVO>> getByPage(StoreEvaluationQueryParams storeEvaluationQueryParams) {

        return ResultUtil.data(memberEvaluationService.queryByParams(storeEvaluationQueryParams));
    }

    @ApiOperation(value = "通过id获取")
    @ApiImplicitParam(name = "id", value = "评价ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<MemberEvaluationVO> get(@PathVariable String id) {
        return ResultUtil.data(memberEvaluationService.queryById(id));
    }

    @ApiOperation(value = "回复评价")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "评价ID", required = true, dataType = "String", paramType = "path"),
            @ApiImplicitParam(name = "reply", value = "回复内容", required = true, dataType = "String", paramType = "query"),
            @ApiImplicitParam(name = "replyImage", value = "回复图片", dataType = "String", paramType = "query")
    })
    @PutMapping(value = "/reply/{id}")
    public ResultMessage<MemberEvaluationVO> reply(@PathVariable String id, @RequestParam String reply, @RequestParam String replyImage) {
        if (memberEvaluationService.reply(id, reply, replyImage)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }
}
