package cn.lili.controller.manager.member;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberGroup;
import cn.lili.modules.member.entity.dos.MemberGroupUser;
import cn.lili.modules.member.service.MemberGroupService;
import cn.lili.modules.member.service.MemberGroupUserService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.*;

import java.util.List;

@RestController
@Tag(name = "管理端,客户分组接口")
@RequestMapping("/manager/member/memberGroup")
public class MemberGroupManagerController {

    @Autowired
    private MemberGroupService memberGroupService;

    @Autowired
    private MemberGroupUserService memberGroupUserService;

    @Operation(description = "通过id获取客户分组")
    @Parameter(name = "id", description = "客户分组ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberGroup> get(@PathVariable String id) {
        return ResultUtil.data(memberGroupService.getById(id));
    }

    @Operation(description = "获取客户分组分页")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<MemberGroup>> getByPage(PageVO page) {
        return ResultUtil.data(memberGroupService.page(PageUtil.initPage(page)));
    }

    @Operation(description = "添加客户分组")
    @Parameter(name = "memberGroup", description = "客户分组")
    @PostMapping
    public ResultMessage<Object> add(@Validated MemberGroup memberGroup) {
        if (memberGroupService.save(memberGroup)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(description = "修改客户分组")
    @Parameter(name = "id", description = "客户分组ID", required = true)
    @Parameter(name = "memberGroup", description = "客户分组")
    @PutMapping("/update/{id}")
    public ResultMessage<Object> update(@PathVariable String id, MemberGroup memberGroup) {
        if (memberGroupService.updateById(memberGroup)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(description = "删除客户分组")
    @Parameter(name = "id", description = "客户分组ID", required = true)
    @DeleteMapping("/delete/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        long count = memberGroupUserService.countByGroupId(id);
        if (count > 0) {
            return ResultUtil.error(ResultCode.ERROR.code(), "请移除分组下的用户后再删除");
        }
        if (memberGroupService.removeById(id)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(description = "给分组添加客户")
    @Parameter(name = "groupId", description = "客户分组ID", required = true)
    @Parameter(name = "memberIds", description = "客户ID列表", required = true)
    @PostMapping("/{groupId}/users")
    public ResultMessage<Object> addUsers(@PathVariable String groupId, @RequestParam List<String> memberIds) {
        if (memberGroupUserService.updateGroupUsers(groupId, memberIds)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(description = "获取分组下客户分页")
    @Parameter(name = "groupId", description = "客户分组ID", required = true)
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/{groupId}/users")
    public ResultMessage<IPage<MemberGroupUser>> getGroupUsers(@PathVariable String groupId, PageVO page) {
        return ResultUtil.data(memberGroupUserService.pageByGroupId(groupId, page));
    }

    @Operation(description = "移除分组中的客户")
    @Parameter(name = "groupId", description = "客户分组ID", required = true)
    @Parameter(name = "memberId", description = "客户ID", required = true)
    @DeleteMapping("/{groupId}/user/{memberId}")
    public ResultMessage<Object> removeUser(@PathVariable String groupId, @PathVariable String memberId) {
        if (memberGroupUserService.removeByGroupAndMember(groupId, memberId)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }

    @Operation(description = "给客户设定分组")
    @Parameter(name = "memberId", description = "客户ID", required = true)
    @Parameter(name = "groupIds", description = "分组ID列表", required = true)
    @PostMapping("/user/{memberId}/groups")
    public ResultMessage<Object> setUserGroups(@PathVariable String memberId, @RequestParam List<String> groupIds) {
        if (memberGroupUserService.updateUserGroups(memberId, groupIds)) {
            return ResultUtil.success(ResultCode.SUCCESS);
        }
        return ResultUtil.error(ResultCode.ERROR);
    }
}
