package cn.lili.controller.manager.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberNotice;
import cn.lili.modules.member.service.MemberNoticeService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,客户站内信管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:31 下午
 */
@RestController
@Tag(name = "管理端,客户站内信管理API")
@RequestMapping("/manager/message/memberNotice")
public class MemberNoticeManagerController {
    @Autowired
    private MemberNoticeService memberNoticeService;

    @Operation(summary = "获取详情")
    @Parameter(name = "id", description = "客户站内信ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<MemberNotice> get(@PathVariable String id) {
        MemberNotice memberNotice = memberNoticeService.getById(id);
        return ResultUtil.data(memberNotice);
    }

    @Operation(summary = "分页获取站内信")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/page")
    public ResultMessage<IPage<MemberNotice>> getByPage(
            PageVO page) {
        IPage<MemberNotice> data = memberNoticeService.page(PageUtil.initPage(page));
        return ResultUtil.data(data);
    }

    @Operation(summary = "阅读消息")
    @Parameter(name = "ids", description = "客户站内信ID列表", required = true)
    @PostMapping("/read/{ids}")
    public ResultMessage<Object> read(@PathVariable List<String> ids) {
        memberNoticeService.read(ids);
        return ResultUtil.success();
    }

    @Operation(summary = "阅读全部")
    @PostMapping("/read/all")
    public ResultMessage<Object> readAll() {
        memberNoticeService.readAll(UserContext.getCurrentUser().getId());
        return ResultUtil.success();
    }

    @Operation(summary = "批量删除")
    @Parameter(name = "ids", description = "客户站内信ID列表", required = true)
    @DeleteMapping("/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        memberNoticeService.removeByIds(ids);
        return ResultUtil.success();
    }

    @Operation(summary = "删除所有")
    @DeleteMapping
    public ResultMessage<Object> deleteAll() {
        memberNoticeService.removeAll(UserContext.getCurrentUser().getId());
        return ResultUtil.success();
    }

}
