package cn.lili.controller.manager.promotion;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.dto.GiftCardCashBatchCreateDTO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashActivitySearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCreateBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCredentialSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashIssueBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardUsageRecordSearchParams;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCredentialVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCreateBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import cn.lili.modules.promotion.service.GiftCardCashService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

/**
 * 管理端现金礼品卡接口（活动、制卡、发卡、凭证查询与导出）
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
@RestController
@Tag(name = "管理端,礼品卡接口")
@RequestMapping("/manager/promotion/giftCardCash")
public class GiftCardCashManagerController {

    @Autowired
    private GiftCardCashService giftCardCashService;

    @Operation(description = "新建礼品卡活动")
    @PostMapping(value = "/activity", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResultMessage<String> saveActivity(@Valid @RequestBody GiftCardCashActivityDTO dto) {
        return ResultUtil.data(giftCardCashService.saveActivity(dto));
    }

    @Operation(description = "更新礼品卡活动")
    @PutMapping(value = "/activity/{id}", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResultMessage<Object> updateActivity(
            @Parameter(description = "活动ID", required = true) @PathVariable String id,
            @Valid @RequestBody GiftCardCashActivityDTO dto) {
        giftCardCashService.updateActivity(id, dto);
        return ResultUtil.success();
    }

    @Operation(description = "删除礼品卡活动（无制卡批次且无凭证时允许）")
    @DeleteMapping("/activity/{id}")
    public ResultMessage<Object> deleteActivity(@Parameter(description = "活动ID", required = true) @PathVariable String id) {
        giftCardCashService.deleteActivity(id);
        return ResultUtil.success();
    }

    @Operation(description = "礼品卡-活动详情")
    @GetMapping("/activity/{id}")
    public ResultMessage<GiftCardCashActivity> getActivity(@PathVariable String id) {
        return ResultUtil.data(giftCardCashService.getActivity(id));
    }

    @Operation(description = "礼品卡-制卡活动分页列表")
    @GetMapping("/activity")
    public ResultMessage<IPage<GiftCardCashActivity>> pageActivity(GiftCardCashActivitySearchParams params, PageVO page) {
        return ResultUtil.data(giftCardCashService.pageActivity(params, page));
    }

    @Operation(description = "在活动下批量制卡（面额与规则以活动为准）")
    @PostMapping(value = "/activity/{activityId}/batchCreate", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResultMessage<String> batchCreate(
            @Parameter(description = "活动ID", required = true) @PathVariable String activityId,
            @Valid @RequestBody GiftCardCashBatchCreateDTO dto) {
        String batchId = giftCardCashService.batchCreateCards(activityId, dto);
        return ResultUtil.data(batchId);
    }

    @Operation(description = "礼品卡-制卡批次分页")
    @GetMapping("/createBatch")
    public ResultMessage<IPage<GiftCardCashCreateBatchVO>> pageCreateBatch(GiftCardCashCreateBatchSearchParams params, PageVO page) {
        return ResultUtil.data(giftCardCashService.pageCreateBatch(params, page));
    }

    @Operation(description = "卡密凭证分页（平台库存+会员资产）")
    @GetMapping("/credential")
    public ResultMessage<IPage<GiftCardCashCredentialVO>> pageCredential(GiftCardCashCredentialSearchParams params, PageVO page) {
        return ResultUtil.data(giftCardCashService.pageCredential(params, page));
    }

    @Operation(description = "礼品卡-发卡/发送记录分页")
    @GetMapping("/issueBatch")
    public ResultMessage<IPage<GiftCardCashIssueBatchVO>> pageIssueBatch(GiftCardCashIssueBatchSearchParams params, PageVO page) {
        return ResultUtil.data(giftCardCashService.pageIssueBatch(params, page));
    }

    @Operation(description = "礼品卡使用记录分页")
    @GetMapping("/usageRecord")
    public ResultMessage<IPage<GiftCardUsageRecordVO>> pageUsageRecord(GiftCardUsageRecordSearchParams params, PageVO page) {
        return ResultUtil.data(giftCardCashService.pageUsageRecord(params, page));
    }

    @Operation(description = "导出某制卡批次的卡号卡密（凭证表）")
    @GetMapping("/createBatch/{createBatchId}/export")
    public void exportCreateBatch(@PathVariable String createBatchId, HttpServletResponse response) {
        giftCardCashService.exportCreateBatch(createBatchId, response);
    }

    @Operation(description = "下载批量发卡 Excel 模板")
    @GetMapping("/issue/template")
    public void downloadIssueTemplate(HttpServletResponse response) {
        giftCardCashService.downloadIssueTemplate(response);
    }

    @Operation(description = "按活动批量发卡")
    @PostMapping(value = "/activity/{activityId}/issueImport", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResultMessage<String> issueImportByActivity(
            @Parameter(description = "活动ID", required = true) @PathVariable String activityId,
            @Parameter(description = "Excel 文件", required = true) @RequestPart("file") MultipartFile file) {
        String issueBatchId = giftCardCashService.issueImportByActivity(activityId, file);
        return ResultUtil.data(issueBatchId);
    }
}
