package cn.lili.controller.seller.goods;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import cn.lili.modules.goods.service.GoodsImportService;
import cn.lili.modules.goods.service.GoodsImportV2Service;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Tag(name = "商品导入")
@RestController
@RequestMapping("/store/goods/import")
public class GoodsImportController {
    @Autowired
    private GoodsImportService goodsImportService;
    @Autowired
    private GoodsImportV2Service goodsImportV2Service;


    @PostMapping(value = "/import", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "上传文件，商品批量添加")
    public ResultMessage<Object> importExcel(@RequestPart("files") MultipartFile files) throws Exception {
        goodsImportService.importExcel(files);
        return ResultUtil.success(ResultCode.SUCCESS);
    }


    @Operation(summary = "下载导入模板", description = "下载导入模板")
    @GetMapping("/downLoad")
    public void download() {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();

        goodsImportService.download(response);
    }


    /**
     * 上传文件，商品批量添加（多规格v2）
     * 按商品货号分组导入多规格商品，返回成功/失败数量及失败明细
     *
     * @param files 导入的 Excel 文件
     * @return 导入结果（成功数、失败数及失败商品明细）
     */
    @PostMapping(value = "/import/v2", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @Operation(summary = "上传文件，商品批量添加（多规格v2）")
    public ResultMessage<GoodsImportResultVO> importGoodsV2(@RequestPart("files") MultipartFile files) {
        return ResultUtil.data(goodsImportV2Service.importGoods(files));
    }


    /**
     * 下载导入模板（多规格v2）
     */
    @Operation(summary = "下载导入模板（多规格v2）", description = "下载多规格商品批量导入模板")
    @GetMapping("/downLoad/v2")
    public void downloadV2() {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();

        goodsImportV2Service.downloadTemplate(response);
    }
}
