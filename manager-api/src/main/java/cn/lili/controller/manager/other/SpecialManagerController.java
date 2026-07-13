package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.page.entity.dos.Special;
import cn.lili.modules.page.service.SpecialService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.util.List;

/**
 * 管理端,专题活动接口
 *
 * @author Bulbasaur
 * @since 2020/12/7 11:33
 */
@RestController
@Tag(name = "管理端,专题活动接口")
@RequestMapping("/manager/other/special")
public class SpecialManagerController {

    @Autowired
    private SpecialService specialService;

    @Operation(summary = "分页获取专题活动")
    @Parameter(description = "分页参数", required = true)
    @GetMapping
    public ResultMessage<IPage<Special>> getByPage(PageVO page) {
        return ResultUtil.data(specialService.page(PageUtil.initPage(page)));
    }

    @Operation(summary = "根据id获取专题活动详情")
    @Parameter(description = "专题活动ID", required = true)
    @GetMapping("/{id}")
    public ResultMessage<Special> get(
            @PathVariable String id) {
        return ResultUtil.data(specialService.getById(id));
    }

    @Operation(summary = "添加专题活动")
    @Parameter(description = "专题活动信息", required = true)
    @PostMapping
    public ResultMessage<Special> add(@Valid Special special) {
        specialService.save(special);
        return ResultUtil.data(special);
    }

    @Operation(summary = "修改专题活动")
    @Parameter(description = "专题活动ID", required = true)
    @PutMapping("/{id}")
    public ResultMessage<Special> edit(
            @PathVariable String id, @Valid Special special) {
        special.setId(id);
        specialService.updateById(special);
        return ResultUtil.data(special);
    }

    @Operation(summary = "删除专题活动")
    @Parameter(description = "专题活动ID", required = true)
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delAllByIds(
            @PathVariable String id) {
        specialService.removeById(id);
        return ResultUtil.success();
    }

}
