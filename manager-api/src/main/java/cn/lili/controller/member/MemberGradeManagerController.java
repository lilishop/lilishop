package cn.lili.controller.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.service.MemberGradeService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,会员等级接口
 *
 * @author Bulbasaur
 * @since 2021/5/16 11:29 下午
 */
@RestController
@Api(tags = "管理端,会员等级接口")
@RequestMapping("/manager/member/memberGrade")
public class MemberGradeManagerController {

    @Autowired
    private MemberGradeService memberGradeService;

    @ApiOperation(value = "通过id获取会员等级")
    @ApiImplicitParam(name = "id", value = "会员等级ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<MemberGrade> get(@PathVariable String id) {

        return ResultUtil.data(memberGradeService.getById(id));
    }

    @ApiOperation(value = "获取会员等级分页")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<MemberGrade>> getByPage(PageVO page) {

        return ResultUtil.data(memberGradeService.page(PageUtil.initPage(page)));
    }

    @ApiOperation(value = "添加会员等级")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "会员等级ID", required = true, paramType = "path")
    })
    @PostMapping(value = "/add")
    public ResultMessage<Object> daa(@Validated  MemberGrade memberGrade) {
        if (memberGradeService.save(memberGrade)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }

    @ApiOperation(value = "修改会员等级")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "id", value = "会员等级ID", required = true, paramType = "path")
    })
    @PutMapping(value = "/update/{id}")
    public ResultMessage<Object> update(@PathVariable String id,MemberGrade memberGrade) {
        if (memberGradeService.updateById(memberGrade)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }



    @ApiOperation(value = "删除会员等级")
    @ApiImplicitParam(name = "id", value = "会员等级ID", required = true, dataType = "String", paramType = "path")
    @DeleteMapping(value = "/delete/{id}")
    public ResultMessage<IPage<Object>> delete(@PathVariable String id) {
        if(memberGradeService.getById(id).getIsDefault()){
            throw new ServiceException(ResultCode.USER_GRADE_IS_DEFAULT);
        }else if(memberGradeService.removeById(id)){
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        throw new ServiceException(ResultCode.ERROR);
    }
}
