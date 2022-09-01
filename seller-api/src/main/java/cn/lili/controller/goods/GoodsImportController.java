package cn.lili.controller.goods;

import cn.lili.common.context.ThreadContextHolder;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.service.GoodsImportService;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@Api(tags = "商品导入")
@RestController
@RequestMapping("/store/goods/import")
public class GoodsImportController {
    @Autowired
    private GoodsImportService goodsImportService;


    @PostMapping(value = "/import", consumes = MediaType.MULTIPART_FORM_DATA_VALUE)
    @ApiOperation(value = "上传文件，商品批量添加")
    public ResultMessage<Object> importExcel(@RequestPart("files") MultipartFile files) {
        try {
            goodsImportService.importExcel(files);
            return ResultUtil.success(ResultCode.SUCCESS);
        } catch (Exception e) {
            e.printStackTrace();
            throw new ServiceException(ResultCode.ERROR);
        }

    }


    @ApiOperation(value = "下载导入模板", produces = "application/octet-stream")
    @GetMapping(value = "/downLoad")
    public void download() {
        HttpServletResponse response = ThreadContextHolder.getHttpResponse();

        goodsImportService.download(response);
    }
}
