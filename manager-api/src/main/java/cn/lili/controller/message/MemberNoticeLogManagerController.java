package cn.lili.controller.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberNoticeLog;
import cn.lili.modules.member.service.MemberNoticeLogService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,会员消息接口
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@RestController
@Api(tags = "管理端,会员消息接口")
@RequestMapping("/manager/memberNoticeLog")
public class MemberNoticeLogManagerController {
    @Autowired
    private MemberNoticeLogService memberNoticeLogService;

    @ApiOperation(value = "通过id获取")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<MemberNoticeLog> get(@PathVariable String id) {
        MemberNoticeLog memberNoticeLog = memberNoticeLogService.getById(id);
        return ResultUtil.data(memberNoticeLog);
    }

    @ApiOperation(value = "获取全部数据")
    @GetMapping(value = "/getAll")
    public ResultMessage<List<MemberNoticeLog>> getAll() {
        List<MemberNoticeLog> list = memberNoticeLogService.list();
        return ResultUtil.data(list);
    }

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<MemberNoticeLog>> getByPage(PageVO page) {
        IPage<MemberNoticeLog> data = memberNoticeLogService.page(PageUtil.initPage(page));
        return ResultUtil.data(data);
    }

    @ApiOperation(value = "编辑或更新数据")
    @PostMapping(value = "/insertOrUpdate")
    public ResultMessage<MemberNoticeLog> saveOrUpdate(MemberNoticeLog memberNoticeLog) {
        memberNoticeLogService.saveOrUpdate(memberNoticeLog);
        return ResultUtil.data(memberNoticeLog);
    }

    @ApiOperation(value = "批量删除")
    @DeleteMapping(value = "/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {
        memberNoticeLogService.removeByIds(ids);
        return ResultUtil.success();
    }
}
