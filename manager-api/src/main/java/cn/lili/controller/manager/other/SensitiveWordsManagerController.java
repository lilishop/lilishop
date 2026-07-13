package cn.lili.controller.manager.other;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dos.SensitiveWords;
import cn.lili.modules.system.service.SensitiveWordsService;
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
 * 管理端,敏感词管理接口
 *
 * @author Bulbasaur
 * @since 2020-05-06 15:18:56
 */
@RestController
@Tag(name = "管理端,敏感词管理接口")
@RequestMapping("/manager/other/sensitiveWords")
public class SensitiveWordsManagerController {

    @Autowired
    private SensitiveWordsService sensitiveWordsService;

    @Operation(summary = "通过id获取")
    @GetMapping("/{id}")
    public ResultMessage<SensitiveWords> get(
            @Parameter(description = "敏感词ID", required = true) @PathVariable String id) {
        return ResultUtil.data(sensitiveWordsService.getById(id));
    }

    @Operation(summary = "分页获取")
    @GetMapping
    public ResultMessage<IPage<SensitiveWords>> getByPage(PageVO page) {
        return ResultUtil.data(sensitiveWordsService.page(PageUtil.initPage(page)));
    }

    @Operation(summary = "添加敏感词")
    @PostMapping
    public ResultMessage<SensitiveWords> add(@Valid SensitiveWords sensitiveWords) {
        sensitiveWordsService.save(sensitiveWords);
        sensitiveWordsService.resetCache();
        return ResultUtil.data(sensitiveWords);
    }

    @Operation(summary = "修改敏感词")
    @PutMapping("/{id}")
    public ResultMessage<SensitiveWords> edit(
            @Parameter(description = "敏感词ID", required = true) @PathVariable String id, SensitiveWords sensitiveWords) {
        sensitiveWords.setId(id);
        sensitiveWordsService.updateById(sensitiveWords);
        sensitiveWordsService.resetCache();
        return ResultUtil.data(sensitiveWords);
    }

    @Operation(summary = "批量删除")
    @DeleteMapping("/delByIds/{ids}")
    public ResultMessage<Object> delAllByIds(
            @Parameter(description = "敏感词ID", required = true) @PathVariable List<String> ids) {
        sensitiveWordsService.removeByIds(ids);
        sensitiveWordsService.resetCache();
        return ResultUtil.success();
    }
}
