package cn.lili.modules.goods.support;

import cn.hutool.core.util.NumberUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsStockTypeEnum;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellStyle;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.DateUtil;
import org.apache.poi.ss.usermodel.Font;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.VerticalAlignment;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

/**
 * 商品 SKU 库存 Excel 导入导出支持类
 * <p>
 * 封装库存数据导出、模板下载与 Excel 批量更新的表格解析渲染逻辑，从业务服务中隔离 Apache POI 细节。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Slf4j
@Component
public class GoodsSkuStockExcelSupport {

    @Autowired
    private GoodsSkuMapper goodsSkuMapper;

    @Autowired
    private GoodsSkuStockManager stockManager;

    /**
     * 导出商品 SKU 库存数据至 Excel 文件
     *
     * @param response     HTTP 响应
     * @param searchParams 商品查询过滤条件
     */
    public void queryExportStock(HttpServletResponse response, GoodsSearchParams searchParams) {
        List<GoodsSkuStockDTO> goodsSkuStockDTOList = this.goodsSkuMapper.queryStocks(searchParams.queryWrapper());
        XSSFWorkbook workbook = initStockExportData(goodsSkuStockDTOList);
        try {
            String fileName = URLEncoder.encode("商品库存", StandardCharsets.UTF_8);
            response.setContentType("application/vnd.ms-excel;charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + fileName + ".xlsx");

            ServletOutputStream out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
            log.error("商品库存导出异常", e);
        } finally {
            try {
                workbook.close();
            } catch (Exception e) {
                log.error("商品库存导出关闭workbook异常", e);
            }
        }
    }

    /**
     * 下载商品库存导入 Excel 模板
     *
     * @param response HTTP 响应
     */
    public void downloadStockTemplate(HttpServletResponse response) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        this.createTemplate(workbook);
        try {
            String fileName = URLEncoder.encode("商品库存导入模板", StandardCharsets.UTF_8);
            response.setContentType("application/vnd.openxmlformats-officedocument.spreadsheetml.sheet;charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + fileName + ".xlsx");

            ServletOutputStream out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
            log.error("商品库存导入模板异常", e);
            throw new ServiceException(ResultCode.ERROR);
        } finally {
            try {
                workbook.close();
            } catch (Exception e) {
                log.error("商品库存导入模板关闭workbook异常", e);
            }
        }
    }

    /**
     * 解析上传的库存 Excel 文件并批量更新 SKU 库存
     *
     * @param storeId 店铺 ID
     * @param file    上传的 Excel 文件
     */
    public void importStock(String storeId, MultipartFile file) {
        List<GoodsSkuStockDTO> goodsSkuStockDTOList = new ArrayList<>();
        try (InputStream inputStream = file.getInputStream()) {
            Workbook workbook = WorkbookFactory.create(inputStream);
            Sheet sheet = workbook.getSheetAt(0);

            if (sheet.getPhysicalNumberOfRows() > 10002) {
                throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "Excel超过10002条");
            }
            Iterator<Row> rowIterator = sheet.rowIterator();
            int rowIndex = 0;
            while (rowIterator.hasNext()) {
                Row row = rowIterator.next();
                rowIndex++;

                if (rowIndex < 3) {
                    continue;
                }

                List<Object> objects = new ArrayList<>();
                for (int i = 0; i < 4; i++) {
                    objects.add(getCellValue(row.getCell(i)));
                }
                log.error(getCellValue(row.getCell(2)));
                log.error(getCellValue(row.getCell(3)));
                if (!"加".equals(getCellValue(row.getCell(2))) && !"减".equals(getCellValue(row.getCell(2)))) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "修改方向数据必须填写“加”或者“减”");
                } else if (!NumberUtil.isInteger(getCellValue(row.getCell(3))) || Integer.parseInt(getCellValue(row.getCell(3))) < 0) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "库存必须为正整数");
                } else if (this.goodsSkuMapper.selectCount(new LambdaQueryWrapper<GoodsSku>()
                        .eq(GoodsSku::getGoodsId, getCellValue(row.getCell(0)))
                        .eq(GoodsSku::getId, getCellValue(row.getCell(1)))
                        .eq(GoodsSku::getStoreId, storeId)) == 0) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "第" + rowIndex + "行商品信息不存在");
                }
                GoodsSkuStockDTO goodsSkuStockDTO = new GoodsSkuStockDTO();
                goodsSkuStockDTO.setGoodsId(getCellValue(row.getCell(0)));
                goodsSkuStockDTO.setSkuId(getCellValue(row.getCell(1)));
                goodsSkuStockDTO.setType(GoodsStockTypeEnum.fromDescription(getCellValue(row.getCell(2))).name());
                goodsSkuStockDTO.setQuantity(Integer.parseInt(getCellValue(row.getCell(3))));
                goodsSkuStockDTOList.add(goodsSkuStockDTO);
            }
        } catch (IOException e) {
            log.error("IOException occurred while processing the Excel file.", e);
            throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, e.getMessage());
        }

        this.stockManager.updateStocksByType(goodsSkuStockDTOList);
    }

    /**
     * 获取 Excel 单元格文本内容
     *
     * @param cell 单元格
     * @return 单元格文本表示
     */
    private String getCellValue(Cell cell) {
        if (cell == null) {
            return "";
        }
        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (org.apache.poi.ss.usermodel.DateUtil.isCellDateFormatted(cell)) {
                    return cell.getDateCellValue().toString();
                } else {
                    double numericValue = cell.getNumericCellValue();
                    if (numericValue == (long) numericValue) {
                        return String.valueOf((long) numericValue);
                    } else {
                        return String.valueOf(numericValue);
                    }
                }
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            case FORMULA:
                return cell.getCellFormula();
            default:
                return "";
        }
    }

    /**
     * 初始化导出用的多 Sheet 工作簿
     *
     * @param goodsSkuStockDTOList 待导出的库存列表
     * @return 填充完成的 XSSFWorkbook
     */
    private XSSFWorkbook initStockExportData(List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        this.createTemplate(workbook);
        this.skuStockList(workbook, goodsSkuStockDTOList);
        this.skuList(workbook, goodsSkuStockDTOList);
        return workbook;
    }

    /**
     * 创建库存编辑模板 Sheet 与表头验证规则
     *
     * @param workbook 工作簿
     */
    private void createTemplate(XSSFWorkbook workbook) {
        Sheet templateSheet = workbook.createSheet("商品库存编辑模板");

        Row description = templateSheet.createRow(0);
        description.setHeightInPoints(90);
        Cell descriptionCell = description.createCell(0);
        descriptionCell.setCellValue("填写说明（请勿删除本说明）：\n" +
                "1.可批量设置多个商品的库存，一次最多10000行\n" +
                "2.库存修改方向：选择库存方向后，会在原先库存基础上进行增加或者减少\n" +
                "3.库存变更数量：需要为整数，不能填写0和负数");
        templateSheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 3));

        CellStyle descriptionStyle = workbook.createCellStyle();
        descriptionStyle.setWrapText(true);
        descriptionStyle.setAlignment(HorizontalAlignment.LEFT);
        descriptionStyle.setVerticalAlignment(VerticalAlignment.CENTER);
        descriptionCell.setCellStyle(descriptionStyle);

        Row header = templateSheet.createRow(1);
        String[] headers = {"商品ID（必填）", "skuID（必填）", "修改方向（必填，只选加 或者 减）", "变更数量（必填）"};

        CellStyle headerStyle = workbook.createCellStyle();
        Font headerFont = workbook.createFont();
        headerFont.setBold(true);
        headerStyle.setFont(headerFont);
        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
            cell.setCellStyle(headerStyle);
        }

        templateSheet.setColumnWidth(0, 30 * 256);
        templateSheet.setColumnWidth(1, 30 * 256);
        templateSheet.setColumnWidth(2, 40 * 256);
        templateSheet.setColumnWidth(3, 25 * 256);

        DataValidationHelper validationHelper = templateSheet.getDataValidationHelper();
        DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(new String[]{"加", "减"});
        CellRangeAddressList addressList = new CellRangeAddressList(2, 10001, 2, 2);
        DataValidation validation = validationHelper.createValidation(constraint, addressList);
        validation.setSuppressDropDownArrow(true);
        validation.setShowErrorBox(true);
        templateSheet.addValidationData(validation);
    }

    /**
     * 填充导出数据中的 SKU 库存明细 Sheet
     *
     * @param workbook             工作簿
     * @param goodsSkuStockDTOList 导出的库存列表
     */
    private void skuStockList(XSSFWorkbook workbook, List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        Sheet skuListSheet = workbook.createSheet("商品库存信息");
        Row header = skuListSheet.createRow(0);
        String[] headers = {"商品ID", "商品名称", "规格ID(SKUID)", "规格", "商品编号", "当前库存"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }

        for (int i = 0; i < goodsSkuStockDTOList.size(); i++) {
            GoodsSkuStockDTO dto = goodsSkuStockDTOList.get(i);
            Row row = skuListSheet.createRow(i + 1);
            row.createCell(0).setCellValue(dto.getGoodsId());
            row.createCell(1).setCellValue(dto.getGoodsName());
            row.createCell(2).setCellValue(dto.getSkuId());
            row.createCell(3).setCellValue(dto.getSimpleSpecs());
            row.createCell(4).setCellValue(dto.getSn());
            row.createCell(5).setCellValue(dto.getQuantity() != null ? dto.getQuantity() : 0);
        }

        skuListSheet.setColumnWidth(0, 30 * 256);
        skuListSheet.setColumnWidth(1, 30 * 256);
        skuListSheet.setColumnWidth(2, 30 * 256);
        skuListSheet.setColumnWidth(3, 30 * 256);
        skuListSheet.setColumnWidth(4, 30 * 256);
        skuListSheet.setColumnWidth(5, 15 * 256);
    }

    /**
     * 填充导出数据中的 SKU 规格信息 Sheet
     *
     * @param workbook             工作簿
     * @param goodsSkuStockDTOList 导出的库存列表
     */
    private void skuList(XSSFWorkbook workbook, List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        Sheet skuListSheet = workbook.createSheet("商品规格信息");
        Row header = skuListSheet.createRow(0);
        String[] headers = {"商品ID", "商品名称", "规格ID(SKUID)", "规格", "商品编号"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }

        for (int i = 0; i < goodsSkuStockDTOList.size(); i++) {
            GoodsSkuStockDTO dto = goodsSkuStockDTOList.get(i);
            Row row = skuListSheet.createRow(i + 1);
            row.createCell(0).setCellValue(dto.getGoodsId());
            row.createCell(1).setCellValue(dto.getGoodsName());
            row.createCell(2).setCellValue(dto.getSkuId());
            row.createCell(3).setCellValue(dto.getSimpleSpecs());
            row.createCell(4).setCellValue(dto.getSn());
        }

        skuListSheet.setColumnWidth(0, 30 * 256);
        skuListSheet.setColumnWidth(1, 30 * 256);
        skuListSheet.setColumnWidth(2, 30 * 256);
        skuListSheet.setColumnWidth(3, 30 * 256);
        skuListSheet.setColumnWidth(4, 30 * 256);
    }
}
