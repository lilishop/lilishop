package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.vo.GoodsImportResultVO;
import org.springframework.web.multipart.MultipartFile;

import jakarta.servlet.http.HttpServletResponse;

/**
 * 商品批量导入v2业务层：多规格商品 Excel 批量导入
 *
 * @author Bulbasaur
 * @since 2026-10-20
 */
public interface GoodsImportV2Service {

    /**
     * 批量导入多规格商品
     *
     * @param file 商品导入 Excel 文件（xlsx，第 1 行说明、第 2 行表头、第 3 行起数据）
     * @return 导入结果，成功/失败按商品组计数
     */
    GoodsImportResultVO importGoods(MultipartFile file);

    /**
     * 下载商品批量导入模板
     *
     * @param response HTTP 响应，用于写出 xlsx 模板文件
     */
    void downloadTemplate(HttpServletResponse response);
}
