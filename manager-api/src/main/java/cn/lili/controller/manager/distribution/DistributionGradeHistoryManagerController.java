package cn.lili.controller.manager.distribution;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dto.DistributionGradeHistorySearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionGradeHistoryVO;
import cn.lili.modules.distribution.service.DistributionGradeHistoryService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * 管理端，分销等级变更历史接口
 *
 * @author Bulbasaur
 * @since 2026-08-05
 */
@RestController
@Tag(name = "管理端,分销等级变更历史接口")
@RequestMapping("/manager/distribution/gradeHistory")
public class DistributionGradeHistoryManagerController {

    @Autowired
    private DistributionGradeHistoryService gradeHistoryService;

    @Operation(summary = "等级变更历史分页")
    @GetMapping
    public ResultMessage<IPage<DistributionGradeHistoryVO>> page(DistributionGradeHistorySearchParams params) {
        return ResultUtil.data(gradeHistoryService.historyPage(params));
    }
}
