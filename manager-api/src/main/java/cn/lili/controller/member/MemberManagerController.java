package cn.lili.controller.member;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ManagerMemberEditDTO;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.service.MemberService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiImplicitParam;
import io.swagger.annotations.ApiImplicitParams;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import javax.validation.Valid;
import java.util.List;

/**
 * 管理端,会员接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "管理端,会员接口")
@RequestMapping("/manager/member")
public class MemberManagerController {
    @Autowired
    private MemberService memberService;

    @ApiOperation(value = "会员分页列表")
    @GetMapping
    public ResultMessage<IPage<MemberVO>> getByPage(MemberSearchVO memberSearchVO, PageVO page) {
        return ResultUtil.data(memberService.getMemberPage(memberSearchVO, page));
    }


    @ApiOperation(value = "通过ID获取会员信息")
    @ApiImplicitParam(name = "id", value = "会员ID", required = true, dataType = "String", paramType = "path")
    @GetMapping(value = "/{id}")
    public ResultMessage<Member> get(@PathVariable String id) {

        return ResultUtil.data(memberService.getById(id));
    }

    @ApiOperation(value = "添加会员")
    @PostMapping
    public ResultMessage<Member> save(@Valid MemberAddDTO member) {

        return ResultUtil.data(memberService.addMember(member));
    }

    @DemoSite
    @PreventDuplicateSubmissions
    @ApiOperation(value = "修改会员基本信息")
    @PutMapping
    public ResultMessage<Member> update(@Valid ManagerMemberEditDTO managerMemberEditDTO) {
        return ResultUtil.data(memberService.updateMember(managerMemberEditDTO));
    }

    @DemoSite
    @PreventDuplicateSubmissions
    @ApiOperation(value = "修改会员状态,开启关闭会员")
    @ApiImplicitParams({
            @ApiImplicitParam(name = "memberIds", value = "会员ID", required = true, dataType = "String", allowMultiple = true, paramType = "query"),
            @ApiImplicitParam(name = "disabled", required = true, dataType = "boolean", paramType = "query")
    })
    @PutMapping("/updateMemberStatus")
    public ResultMessage<Object> updateMemberStatus(@RequestParam List<String> memberIds, @RequestParam Boolean disabled) {
        memberService.updateMemberStatus(memberIds, disabled);
        return ResultUtil.success();
    }


    @ApiOperation(value = "根据条件查询会员总数")
    @GetMapping("/num")
    public ResultMessage<Long> getByPage(MemberSearchVO memberSearchVO) {
        return ResultUtil.data(memberService.getMemberNum(memberSearchVO));
    }


}
