package cn.lili.controller.manager.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.vo.EnumValueVO;
import cn.lili.modules.member.entity.vo.MemberBenefitDetailVO;
import cn.lili.modules.member.entity.vo.MemberBenefitGrantRecordVO;
import cn.lili.modules.member.service.MemberBenefitService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@Tag(name = "管理端,客户权益接口")
@RequestMapping("/manager/member/benefit")
public class MemberBenefitManagerController {

    @Autowired
    private MemberBenefitService memberBenefitService;

    @Operation(description = "通过id获取客户权益详情")
    @Parameter(name = "id", description = "客户权益ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberBenefitDetailVO> get(@PathVariable String id) {
        return ResultUtil.data(memberBenefitService.getManagerBenefitDetail(id));
    }

    @Operation(description = "获取客户权益分页")
    @Parameter(name = "keyword", description = "关键字(权益名称)", required = false)
    @Parameter(name = "state", description = "状态 OPEN/CLOSE", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping("/getByPage")
    public ResultMessage<IPage<MemberBenefit>> getByPage(@RequestParam(required = false) String keyword,
                                                         @RequestParam(required = false) String state,
                                                         PageVO page) {
        return ResultUtil.data(memberBenefitService.getBenefitPage(keyword, state, page));
    }

    @Operation(description = "权益发放记录分页（会员达到等级时触发的一次性发放）")
    @Parameter(name = "mobile", description = "会员手机号（精确）", required = false)
    @Parameter(name = "gradeId", description = "等级ID", required = false)
    @Parameter(name = "page", description = "分页参数", required = false)
    @GetMapping("/grantRecords/getByPage")
    public ResultMessage<IPage<MemberBenefitGrantRecordVO>> grantRecordsGetByPage(
            @RequestParam(required = false) String mobile,
            @RequestParam(required = false) String gradeId,
            PageVO page) {
        return ResultUtil.data(memberBenefitService.pageBenefitGrantRecords(mobile, gradeId, page));
    }

    @Operation(description = "获取客户权益列表")
    @Parameter(name = "onlyOpen", description = "仅返回启用权益", required = false)
    @GetMapping("/list")
    public ResultMessage<List<MemberBenefit>> list(@RequestParam(required = false) Boolean onlyOpen) {
        return ResultUtil.data(memberBenefitService.listBenefits(onlyOpen));
    }

    @Operation(description = "获取客户权益类型列表")
    @GetMapping("/types")
    public ResultMessage<List<EnumValueVO>> types() {
        return ResultUtil.data(memberBenefitService.listBenefitTypes());
    }

    @Operation(description = "添加客户权益")
    @Parameter(name = "memberBenefit", description = "客户权益")
    @PostMapping
    public ResultMessage<Object> add(@Validated MemberBenefit memberBenefit) {
        memberBenefitService.saveBenefit(memberBenefit);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "修改客户权益")
    @Parameter(name = "id", description = "客户权益ID", required = true)
    @Parameter(name = "memberBenefit", description = "客户权益")
    @PutMapping("/update/{id}")
    public ResultMessage<Object> update(@PathVariable String id, @Validated MemberBenefit memberBenefit) {
        memberBenefitService.updateBenefit(id, memberBenefit);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "删除客户权益")
    @Parameter(name = "id", description = "客户权益ID", required = true)
    @DeleteMapping("/delete/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        memberBenefitService.deleteBenefit(id);
        return ResultUtil.success(ResultCode.SUCCESS);
    }

    @Operation(description = "切换客户权益开关")
    @Parameter(name = "id", description = "客户权益ID", required = true)
    @Parameter(name = "state", description = "状态 OPEN/CLOSE", required = true)
    @PutMapping("/state/{id}")
    public ResultMessage<Object> updateState(@PathVariable String id, @RequestParam String state) {
        memberBenefitService.updateBenefitState(id, state);
        return ResultUtil.success(ResultCode.SUCCESS);
    }
}
