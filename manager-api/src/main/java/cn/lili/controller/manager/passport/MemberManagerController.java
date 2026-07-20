package cn.lili.controller.manager.passport;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dto.ManagerMemberEditDTO;
import cn.lili.modules.member.entity.dto.MemberAddDTO;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.entity.vo.MemberSearchVO;
import cn.lili.modules.member.entity.vo.MemberVO;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.aspect.annotation.SystemLogPoint;
import cn.lili.modules.wallet.entity.dto.MemberWalletUpdateDTO;
import cn.lili.modules.wallet.entity.enums.DepositServiceTypeEnum;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

/**
 * 管理端,客户接口
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "管理端,客户接口")
@RequestMapping("/manager/passport/member")
public class MemberManagerController {
    @Autowired
    private MemberService memberService;

    @Operation(description = "客户分页列表")
    @Parameter(name = "memberSearchVO", description = "客户查询参数", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping
    public ResultMessage<IPage<MemberVO>> getByPage(MemberSearchVO memberSearchVO, PageVO page) {
        return ResultUtil.data(memberService.getMemberPage(memberSearchVO, page));
    }


    @Operation(description = "通过ID获取客户信息")
    @Parameter(name = "id", description = "客户ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<MemberVO> get(@PathVariable String id) {

        return ResultUtil.data(memberService.getMember(id));
    }

    @Operation(description = "添加客户")
    @SystemLogPoint(description = "添加客户", customerLog = "'新增用户名称: ['+#member.username+']'")
    @PostMapping
    public ResultMessage<Member> save(@Valid MemberAddDTO member) {

        return ResultUtil.data(memberService.addMember(member));
    }

    @DemoSite
    @PreventDuplicateSubmissions
    @SystemLogPoint(description = "修改客户信息", customerLog = "'修改的用户名称: ['+#managerMemberEditDTO.username+']'")
    @Operation(description = "修改客户基本信息")
    @Parameter(name = "managerMemberEditDTO", description = "客户修改参数", required = true)
    @PutMapping
    public ResultMessage<Member> update(@Valid ManagerMemberEditDTO managerMemberEditDTO) {
        return ResultUtil.data(memberService.updateMember(managerMemberEditDTO));
    }

    @DemoSite
    @PreventDuplicateSubmissions
    @SystemLogPoint(description = "修改客户状态", customerLog = "'修改的客户名称: ['+#memberIds+']，是否开启: ['+#disabled+']'")
    @Operation(description = "修改客户状态,开启关闭客户")
    @Parameter(name = "memberIds", description = "客户ID", required = true)
    @Parameter(name = "disabled", description = "是否开启", required = true)
    @PutMapping("/updateMemberStatus")
    public ResultMessage<Object> updateMemberStatus(@RequestParam List<String> memberIds, @RequestParam Boolean disabled) {
        memberService.updateMemberStatus(memberIds, disabled);
        return ResultUtil.success();
    }


    @Operation(description = "根据条件查询客户总数")
    @Parameter(name = "memberSearchVO", description = "客户查询参数", required = false)
    @GetMapping("/num")
    public ResultMessage<Long> getByPage(MemberSearchVO memberSearchVO) {
        return ResultUtil.data(memberService.getMemberNum(memberSearchVO));
    }




    @PutMapping("/updateMemberPoint")
    @Operation(summary = "增加用户余额")
    @Parameter(name = "memberId", description = "客户ID", required = true)
    @Parameter(name = "point", description = "积分", required = true)
    @Parameter(name = "type", description = "类型", required = true)
    public ResultMessage<Object> updateMemberPoint(String memberId ,Long point,String type) {
        String content="";
        if (type.equals(PointTypeEnum.INCREASE.name())) {
            content="运营后台手动增加积分:"+point;
        }else{
            content="运营后台手动减少积分:"+point;
        }
        if(memberService.updateMemberPoint(point, type, memberId, content,
                cn.lili.modules.member.entity.enums.PointSourceEnum.ADMIN.name())){
            return ResultUtil.success();
        }
        return ResultUtil.error();
    }

}
