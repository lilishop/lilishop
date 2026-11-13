package cn.lili.modules.goods.support;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.hutool.poi.excel.ExcelWriter;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CardKey;
import cn.lili.modules.goods.entity.dto.CardKeyImportDTO;
import cn.lili.modules.goods.entity.dto.search.CardKeySearchParams;
import cn.lili.modules.goods.entity.vo.CardKeyImportResultVO;
import cn.lili.modules.goods.util.CardKeySecretUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 电子卡券卡密导入导出与模板下载支持类
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
public class CardKeyImportSupport {

    /**
     * 单次导入最大允许行数。
     */
    public static final int IMPORT_MAX_ROWS = 10000;

    /**
     * 单次导出最大允许行数。
     */
    public static final int EXPORT_MAX_ROWS = 10000;

    /**
     * 解析卡密导入文件。
     * 从第 2 行起读取卡号与卡密（跳过表头）。
     *
     * @param file 包含卡号与卡密的 Excel 文件
     * @return 解析后的卡密行 DTO 列表
     */
    public static List<CardKeyImportDTO> parseImportFile(MultipartFile file) {
        try (ExcelReader reader = ExcelUtil.getReader(file.getInputStream())) {
            // 第 1 行表头，从第 2 行起读数据（Hutool 行号从 0 起，故 startRowIndex = 1）
            List<List<Object>> raw = reader.read(1, reader.getRowCount() - 1);
            List<CardKeyImportDTO> rows = new ArrayList<>();
            int rowNum = 2;
            for (List<Object> line : raw) {
                String cardNo = line.size() > 0 ? Convert.toStr(line.get(0)) : "";
                String cardSecret = line.size() > 1 ? Convert.toStr(line.get(1)) : "";
                rows.add(new CardKeyImportDTO(rowNum++, cardNo, cardSecret));
            }
            return rows;
        } catch (Exception e) {
            log.error("解析卡密导入文件失败", e);
            throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR);
        }
    }

    /**
     * 下载卡密批量导入模板（卡号、卡密两列）。
     *
     * @param response HTTP 响应对象
     */
    public static void downloadImportTemplate(HttpServletResponse response) {
        ExcelWriter writer = ExcelUtil.getWriter(true);
        try {
            writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密"));
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            response.setHeader("Content-Disposition",
                    "attachment;filename=" + URLEncoder.encode("card-key-import-template.xlsx", StandardCharsets.UTF_8));
            ServletOutputStream out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("卡密导入模板下载失败", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
        }
    }

    /**
     * 导出卡密列表至 Excel（支持解密展示明文卡密）。
     *
     * @param params 筛选条件
     * @param list 卡密数据集合
     * @param cardKeySecretUtil 卡密加解密组件
     * @param response HTTP 响应对象
     */
    public static void export(CardKeySearchParams params,
                              List<CardKey> list,
                              CardKeySecretUtil cardKeySecretUtil,
                              HttpServletResponse response) {
        String skuPart = CharSequenceUtil.blankToDefault(params.getSkuId(), "all");
        String filename = "card-key-" + skuPart + "-" + DateUtil.format(new Date(), "yyyyMMddHHmmss") + ".xlsx";
        ExcelWriter writer = ExcelUtil.getWriter(true);
        try {
            writer.writeHeadRow(CollUtil.newArrayList("卡号", "卡密", "状态", "导入时间", "发卡时间", "订单号"));
            for (CardKey cardKey : list) {
                writer.writeRow(CollUtil.newArrayList(
                        cardKey.getCardNo(),
                        cardKeySecretUtil.decrypt(cardKey.getCardSecret()),
                        cardKey.getStatus(),
                        cardKey.getCreateTime() == null ? "" : DateUtil.formatDateTime(cardKey.getCreateTime()),
                        cardKey.getAllocatedTime() == null ? "" : DateUtil.formatDateTime(cardKey.getAllocatedTime()),
                        CharSequenceUtil.blankToDefault(cardKey.getOrderSn(), "")));
            }
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=utf-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + URLEncoder.encode(filename, StandardCharsets.UTF_8));
            ServletOutputStream out = response.getOutputStream();
            writer.flush(out, true);
        } catch (Exception e) {
            log.error("卡池导出失败", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            writer.close();
        }
    }

    /**
     * 校验起止时间范围合法性。
     *
     * @param start 起始时间字符串
     * @param end 截止时间字符串
     */
    public static void validateTimeRange(String start, String end) {
        if (CharSequenceUtil.isNotBlank(start) && CharSequenceUtil.isNotBlank(end)) {
            if (DateUtil.parse(start).after(DateUtil.parse(end))) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "导入时间区间无效");
            }
        }
    }

    /**
     * 根据查询参数构造卡密查询包装器。
     *
     * @param params 查询参数
     * @return LambdaQueryWrapper
     */
    public static LambdaQueryWrapper<CardKey> buildListWrapper(CardKeySearchParams params) {
        LambdaQueryWrapper<CardKey> wrapper = new LambdaQueryWrapper<>();
        wrapper.eq(CardKey::getStoreId, params.getStoreId());
        wrapper.eq(CharSequenceUtil.isNotBlank(params.getSkuId()), CardKey::getSkuId, params.getSkuId());
        wrapper.eq(CharSequenceUtil.isNotBlank(params.getGoodsId()), CardKey::getGoodsId, params.getGoodsId());
        wrapper.eq(CharSequenceUtil.isNotBlank(params.getStatus()), CardKey::getStatus, params.getStatus());
        wrapper.like(CharSequenceUtil.isNotBlank(params.getCardNo()), CardKey::getCardNo, params.getCardNo());
        if (CharSequenceUtil.isNotBlank(params.getCreateTimeStart())) {
            wrapper.ge(CardKey::getCreateTime, DateUtil.parse(params.getCreateTimeStart()));
        }
        if (CharSequenceUtil.isNotBlank(params.getCreateTimeEnd())) {
            wrapper.le(CardKey::getCreateTime, DateUtil.parse(params.getCreateTimeEnd()));
        }
        wrapper.orderByDesc(CardKey::getCreateTime);
        return wrapper;
    }

    /**
     * 记录导入单行失败信息。
     *
     * @param result 导入统计结果
     * @param row 文件行号
     * @param cardNo 卡号
     * @param reason 失败原因
     */
    public static void addFailRow(CardKeyImportResultVO result, Integer row, String cardNo, String reason) {
        CardKeyImportResultVO.FailRow failRow = new CardKeyImportResultVO.FailRow();
        failRow.setRow(row);
        failRow.setCardNo(cardNo);
        failRow.setReason(reason);
        result.getFailRows().add(failRow);
    }
}
