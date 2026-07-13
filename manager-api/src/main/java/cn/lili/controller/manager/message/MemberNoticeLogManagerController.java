package cn.lili.controller.manager.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberNoticeLog;
import cn.lili.modules.member.service.MemberNoticeLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import io.swagger.v3.oas.annotations.Parameter;

import java.util.List;

/**
 * 管理端,客户消息接口
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "管理端,客户消息接口")
@RequestMapping("/manager/message/memberNoticeLog")
public class MemberNoticeLogManagerController {
    @Autowired
    private MemberNoticeLogService memberNoticeLogService;

    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "客户消息日志ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberNoticeLog> get(@PathVariable String id) {
        MemberNoticeLog memberNoticeLog = memberNoticeLogService.getById(id);
        return ResultUtil.data(memberNoticeLog);
    }

    @Operation(summary = "获取全部数据")
    @GetMapping("/getAll")
    public ResultMessage<List<MemberNoticeLog>> getAll() {
        List<MemberNoticeLog> list = memberNoticeLogService.list();
        return ResultUtil.data(list);
    }

    @Operation(summary = "分页获取")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<MemberNoticeLog>> getByPage(PageVO page) {
        IPage<MemberNoticeLog> data = memberNoticeLogService.page(PageUtil.initPage(page));
        return ResultUtil.data(data);
    }

    @Operation(summary = "编辑或更新数据")
    @Parameter(name = "memberNoticeLog", description = "客户消息日志实体")
    @PostMapping("/insertOrUpdate")
    public ResultMessage<MemberNoticeLog> saveOrUpdate(MemberNoticeLog memberNoticeLog) {
        memberNoticeLogService.saveOrUpdate(memberNoticeLog);
        return ResultUtil.data(memberNoticeLog);
    }

    @Operation(summary = "批量删除")
    @Parameter(name = "ids", description = "客户消息日志ID列表", required = true)
    @DeleteMapping("/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        memberNoticeLogService.removeByIds(ids);
        return ResultUtil.success();
    }
}
