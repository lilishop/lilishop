package cn.lili.modules.goods.service;

import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletResponse;

public interface GoodsImportService {


    /**
     * 下载导入列表
     * @param response
     */
    void download(HttpServletResponse response);

    /**
     * 导入商品
     */
    void importExcel(MultipartFile files) throws Exception;
}
