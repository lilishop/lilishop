package cn.lili.controller.seller.goods;

import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dto.CardKeyAddDTO;
import cn.lili.modules.goods.entity.dto.search.CardKeySearchParams;
import cn.lili.modules.goods.entity.vo.CardKeyImportResultVO;
import cn.lili.modules.goods.entity.vo.CardKeyPoolVO;
import cn.lili.modules.goods.entity.vo.CardKeyStatsVO;
import cn.lili.modules.goods.service.CardKeyService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

/**
 * 店铺端卡密管理 HTTP 接口。
 * <p>
 * Base Path: {@code /store/goods/card-key}。卡密明文仅在本端授权上下文返回，禁止写入日志。
 *
 * @author Mike
 * @date 2026-07-31
 */
@RestController
@Tag(name = "店铺端,卡密管理")
@RequestMapping("/store/goods/card-key")
public class CardKeyStoreController {

    @Autowired
    private CardKeyService cardKeyService;

    /**
     * 动态生成 Excel 模板，仅表头「卡号」「卡密」。
     */
    @Operation(summary = "下载卡密导入模板")
    @GetMapping("/import/template")
    public void downloadImportTemplate(HttpServletResponse response) {
        cardKeyService.downloadImportTemplate(response);
    }

    /**
     * multipart 上传，单次最多 10,000 行；部分失败返回 failRows。
     */
    @Operation(summary = "批量导入卡密")
    @PostMapping(value = "/import", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    public ResultMessage<CardKeyImportResultVO> importCardKeys(@RequestParam String skuId,
                                                               @RequestParam("file") MultipartFile file) {
        return ResultUtil.data(cardKeyService.importExcel(skuId, file));
    }

    @Operation(summary = "单条新增卡密")
    @PostMapping("/add")
    public ResultMessage<CardKeyPoolVO> add(@Valid @RequestBody CardKeyAddDTO dto) {
        return ResultUtil.data(cardKeyService.addCardKey(dto));
    }

    @Operation(summary = "卡池分页列表")
    @GetMapping("/list")
    public ResultMessage<IPage<CardKeyPoolVO>> list(CardKeySearchParams params) {
        return ResultUtil.data(cardKeyService.page(params));
    }

    @Operation(summary = "作废卡密")
    @PutMapping("/void/{id}")
    public ResultMessage<Object> voidCard(@PathVariable String id) {
        cardKeyService.voidCard(id);
        return ResultUtil.success();
    }

    @Operation(summary = "卡池状态统计")
    @GetMapping("/stats/{skuId}")
    public ResultMessage<CardKeyStatsVO> stats(@PathVariable String skuId) {
        return ResultUtil.data(cardKeyService.stats(skuId));
    }

    /**
     * 按 list 相同筛选条件同步导出，含明文 cardSecret。
     */
    @Operation(summary = "卡池导出")
    @GetMapping("/export")
    public void export(CardKeySearchParams params, HttpServletResponse response) {
        cardKeyService.export(params, response);
    }
}
