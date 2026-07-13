package cn.lili.controller.manager.message;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.vo.SearchVO;
import cn.lili.modules.member.entity.dos.MemberNoticeSenter;
import cn.lili.modules.member.service.MemberNoticeSenterService;
import io.swagger.v3.oas.annotations.Parameter;

import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.List;

/**
 * 管理端,客户消息接口
 *
 * @author Chopper
 * @since 2020-02-25 14:10:16
 */
@RestController
@Tag(name = "管理端,客户消息接口")
@RequestMapping("/manager/message/memberNoticeSenter")
public class MemberNoticeSenterManagerController {
    @Autowired
    private MemberNoticeSenterService memberNoticeSenterService;

    @Operation(summary = "通过id获取")
    @Parameter(name = "id", description = "客户消息ID", required = true)
    @GetMapping("/get/{id}")
    public ResultMessage<MemberNoticeSenter> get(@PathVariable String id) {
        MemberNoticeSenter memberNoticeSenter = memberNoticeSenterService.getById(id);
        return ResultUtil.data(memberNoticeSenter);
    }

    @Operation(summary = "获取全部数据")
    @GetMapping("/getAll")
    public ResultMessage<List<MemberNoticeSenter>> getAll() {

        List<MemberNoticeSenter> list = memberNoticeSenterService.list();
        return ResultUtil.data(list);
    }

    @Operation(summary = "分页获取")
    @Parameter(name = "entity", description = "客户消息查询实体")
    @Parameter(name = "searchVo", description = "分页查询参数")
    @Parameter(name = "page", description = "分页参数")
    @GetMapping("/getByPage")
    public ResultMessage<IPage<MemberNoticeSenter>> getByPage(MemberNoticeSenter entity,
                                                              SearchVO searchVo,
                                                              PageVO page) {
        return ResultUtil.data(memberNoticeSenterService.getByPage(entity, searchVo, page));
    }

    @Operation(summary = "编辑或更新数据")
    @Parameter(name = "memberNoticeSenter", description = "客户消息实体", required = true)
    @PostMapping("/insertOrUpdate")
    public ResultMessage<MemberNoticeSenter> saveOrUpdate(MemberNoticeSenter memberNoticeSenter) {

        memberNoticeSenterService.customSave(memberNoticeSenter);
        return ResultUtil.data(memberNoticeSenter);
    }

    @Operation(summary = "批量删除")
    @Parameter(name = "ids", description = "客户消息ID列表", required = true)
    @DeleteMapping("/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(@PathVariable List<String> ids) {
        memberNoticeSenterService.removeByIds(ids);
        return ResultUtil.success();
    }
}
