package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.service.DistributionGradeService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

/**
 * 管理端,分销等级接口
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销等级接口")
@RequestMapping("/manager/distribution/grade")
public class DistributionGradeManagerController {

    @Autowired
    private DistributionGradeService distributionGradeService;

    @Operation(summary = "等级列表")
    @GetMapping
    public ResultMessage<List<DistributionGrade>> list() {
        return ResultUtil.data(distributionGradeService.listGrades());
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "批量保存等级")
    @PutMapping("/batch")
    public ResultMessage<Object> saveAll(@Valid @RequestBody List<DistributionGrade> grades) {
        distributionGradeService.saveAllGrades(grades);
        return ResultUtil.success();
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "删除等级")
    @DeleteMapping("/{id}")
    public ResultMessage<Object> delete(@PathVariable String id) {
        distributionGradeService.deleteGrade(id);
        return ResultUtil.success();
    }
}
