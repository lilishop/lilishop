package cn.lili.controller.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.member.entity.dos.MemberNotice;
import cn.lili.modules.member.service.MemberNoticeService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.UpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,会员站内信管理接口
 *
 * @author Chopper
 * @since 2020/11/17 4:31 下午
 */
@RestController
@Api(tags = "管理端,会员站内信管理API")
@RequestMapping("/manager/message/memberNotice")
public class MemberNoticeManagerController {
    @Autowired
    private MemberNoticeService memberNoticeService;

    @ApiOperation(value = "获取详情")
    @GetMapping(value = "/{id}")
    public ResultMessage<MemberNotice> get(@PathVariable String id) {
        MemberNotice memberNotice = memberNoticeService.getById(id);
        return ResultUtil.data(memberNotice);
    }

    @ApiOperation(value = "分页获取站内信")
    @GetMapping(value = "/page")
    public ResultMessage<IPage<MemberNotice>> getByPage(
            PageVO page) {
        IPage<MemberNotice> data = memberNoticeService.page(PageUtil.initPage(page));
        return ResultUtil.data(data);
    }

    @ApiOperation(value = "阅读消息")
    @PostMapping("/read/{ids}")
    public ResultMessage<Object> read(@PathVariable List ids) {
        UpdateWrapper updateWrapper = new UpdateWrapper();
        updateWrapper.in("id", ids);
        updateWrapper.set("is_read", true);
        memberNoticeService.update(updateWrapper);
        return ResultUtil.success();
    }

    @ApiOperation(value = "阅读全部")
    @PostMapping("/read/all")
    public ResultMessage<Object> readAll() {
        UpdateWrapper updateWrapper = new UpdateWrapper();
        updateWrapper.in("member_id", UserContext.getCurrentUser().getId());
        updateWrapper.set("is_read", true);
        memberNoticeService.update(updateWrapper);
        return ResultUtil.success();
    }

    @ApiOperation(value = "批量删除")
    @DeleteMapping(value = "/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {
        memberNoticeService.removeByIds(ids);
        return ResultUtil.success();
    }

    @ApiOperation(value = "删除所有")
    @DeleteMapping
    public ResultMessage<Object> deleteAll() {
        QueryWrapper queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("member_id", UserContext.getCurrentUser().getId());
        memberNoticeService.remove(queryWrapper);
        return ResultUtil.success();
    }

}
