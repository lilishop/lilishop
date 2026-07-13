package cn.lili.controller.manager.member;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberAddress;
import cn.lili.modules.member.service.MemberAddressService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Parameter;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;

/**
 * 管理端,客户地址API
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "管理端,客户地址API")
@RequestMapping("/manager/member/address")
public class MemberAddressManagerController {
    @Autowired
    private MemberAddressService memberAddressService;

    @Operation(description = "客户地址分页列表")
    @GetMapping("/{memberId}")
    public ResultMessage<IPage<MemberAddress>> getByPage(PageVO page, @PathVariable("memberId") String memberId) {
        return ResultUtil.data(memberAddressService.getAddressByMember(page, memberId));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "删除客户收件地址")
    @Parameter(name = "id", description = "客户地址ID", required = true)
    @DeleteMapping("/delById/{id}")
    public ResultMessage<Object> delShippingAddressById(@PathVariable String id) {
        memberAddressService.removeMemberAddress(id);
        return ResultUtil.success();
    }

    @Operation(description = "修改客户收件地址")
    @PutMapping
    public ResultMessage<MemberAddress> editShippingAddress(@Valid MemberAddress shippingAddress) {
        //修改客户地址
        return ResultUtil.data(memberAddressService.updateMemberAddress(shippingAddress));
    }

    @PreventDuplicateSubmissions
    @Operation(description = "新增客户收件地址")
    @PostMapping
    public ResultMessage<MemberAddress> addShippingAddress(@Valid MemberAddress shippingAddress) {
        //添加客户地址
        return ResultUtil.data(memberAddressService.saveMemberAddress(shippingAddress));
    }


}
