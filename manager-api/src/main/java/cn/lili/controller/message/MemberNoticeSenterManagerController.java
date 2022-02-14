package cn.lili.controller.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.entity.dos.MemberNoticeSenter;
import cn.lili.modules.member.service.MemberNoticeSenterService;
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
@RequestMapping("/manager/message/memberNoticeSenter")
public class MemberNoticeSenterManagerController {
    @Autowired
    private MemberNoticeSenterService memberNoticeSenterService;

    @ApiOperation(value = "通过id获取")
    @GetMapping(value = "/get/{id}")
    public ResultMessage<MemberNoticeSenter> get(@PathVariable String id) {
        MemberNoticeSenter memberNoticeSenter = memberNoticeSenterService.getById(id);
        return ResultUtil.data(memberNoticeSenter);
    }

    @ApiOperation(value = "获取全部数据")
    @GetMapping(value = "/getAll")
    public ResultMessage<List<MemberNoticeSenter>> getAll() {

        List<MemberNoticeSenter> list = memberNoticeSenterService.list();
        return ResultUtil.data(list);
    }

    @ApiOperation(value = "分页获取")
    @GetMapping(value = "/getByPage")
    public ResultMessage<IPage<MemberNoticeSenter>> getByPage(MemberNoticeSenter entity,
                                                              SearchVO searchVo,
                                                              PageVO page) {
        IPage<MemberNoticeSenter> data = memberNoticeSenterService.page(PageUtil.initPage(page), PageUtil.initWrapper(entity, searchVo));
        return ResultUtil.data(data);
    }

    @ApiOperation(value = "编辑或更新数据")
    @PostMapping(value = "/insertOrUpdate")
    public ResultMessage<MemberNoticeSenter> saveOrUpdate(MemberNoticeSenter memberNoticeSenter) {

        memberNoticeSenterService.customSave(memberNoticeSenter);
        return ResultUtil.data(memberNoticeSenter);
    }

    @ApiOperation(value = "批量删除")
    @DeleteMapping(value = "/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List ids) {
        memberNoticeSenterService.removeByIds(ids);
        return ResultUtil.success();
    }
}
