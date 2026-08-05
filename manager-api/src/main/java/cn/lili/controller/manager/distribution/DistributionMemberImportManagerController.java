package cn.lili.controller.manager.distribution;

import cn.lili.common.aop.annotation.PreventDuplicateSubmissions;
import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.dos.DistributionImportBatch;
import cn.lili.modules.distribution.entity.dos.DistributionImportItem;
import cn.lili.modules.distribution.service.DistributionMemberImportService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 管理端,分销员批量导入
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销员批量导入接口")
@RequestMapping("/manager/distribution/memberImport")
public class DistributionMemberImportManagerController {

    @Autowired
    private DistributionMemberImportService memberImportService;

    @Operation(summary = "下载分销员导入模板")
    @GetMapping(value = "/template", produces = "application/octet-stream")
    public void downloadTemplate() {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        memberImportService.downloadTemplate(response);
    }

    @PreventDuplicateSubmissions
    @Operation(summary = "批量导入分销员")
    @PostMapping(value = "/import", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResultMessage<DistributionImportBatch> importExcel(@RequestPart("file") MultipartFile file) {
        return ResultUtil.data(memberImportService.importExcel(file));
    }

    @Operation(summary = "导入批次分页")
    @GetMapping("/batch")
    public ResultMessage<IPage<DistributionImportBatch>> pageBatches(PageVO page) {
        return ResultUtil.data(memberImportService.pageBatches(page));
    }

    @Operation(summary = "导入明细分页")
    @GetMapping("/batch/{batchId}/items")
    public ResultMessage<IPage<DistributionImportItem>> pageItems(
            @Parameter(description = "批次ID", required = true) @PathVariable String batchId,
            @Parameter(description = "明细状态 SUCCESS/FAIL") @RequestParam(required = false) String status,
            PageVO page) {
        return ResultUtil.data(memberImportService.pageItems(batchId, status, page));
    }

    @Operation(summary = "下载导入失败列表")
    @GetMapping(value = "/batch/{batchId}/failExport", produces = "application/octet-stream")
    public void downloadFailItems(
            @Parameter(description = "批次ID", required = true) @PathVariable String batchId) {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();
        memberImportService.downloadFailItems(batchId, response);
    }
}
