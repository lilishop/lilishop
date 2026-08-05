package cn.lili.modules.distribution.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.DistributionImportBatch;
import cn.lili.modules.distribution.entity.dos.DistributionImportItem;
import com.baomidou.mybatisplus.core.metadata.IPage;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.multipart.MultipartFile;

/**
 * 分销员批量导入
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionMemberImportService {

    /**
     * 下载导入模板。
     *
     * @param response HTTP 响应
     */
    void downloadTemplate(HttpServletResponse response);

    /**
     * 执行批量导入。
     *
     * @param file 导入文件
     * @return 导入批次
     */
    DistributionImportBatch importExcel(MultipartFile file);

    /**
     * 导入批次分页。
     *
     * @param page 分页参数
     * @return 批次分页
     */
    IPage<DistributionImportBatch> pageBatches(PageVO page);

    /**
     * 导入明细分页。
     *
     * @param batchId 批次ID
     * @param status  明细状态
     * @param page    分页参数
     * @return 明细分页
     */
    IPage<DistributionImportItem> pageItems(String batchId, String status, PageVO page);

    /**
     * 下载某批次失败明细。
     *
     * @param batchId  批次ID
     * @param response HTTP 响应
     */
    void downloadFailItems(String batchId, HttpServletResponse response);
}
