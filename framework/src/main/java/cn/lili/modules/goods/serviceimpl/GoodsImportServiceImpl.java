package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.poi.excel.ExcelReader;
import cn.hutool.poi.excel.ExcelUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.dto.GoodsImportDTO;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.vos.CategoryVO;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.GoodsImportService;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;
import cn.lili.modules.store.service.StoreDetailService;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hssf.usermodel.DVConstraint;
import org.apache.poi.hssf.usermodel.HSSFDataValidation;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class GoodsImportServiceImpl implements GoodsImportService {

    @Autowired
    private FreightTemplateService freightTemplateService;
    @Autowired
    private StoreDetailService storeDetailService;
    @Autowired
    private CategoryService categoryService;
    @Autowired
    private GoodsUnitService goodsUnitService;
    @Autowired
    private GoodsService goodsService;

    @Override
    public void download(HttpServletResponse response) {
        String storeId = "1376369067769724928";
//        //Objects.requireNonNull(UserContext.getCurrentUser()).getStoreId();
        //创建Excel工作薄对象
        Workbook workbook = new HSSFWorkbook();
        //生成一个表格 设置：页签
        Sheet sheet = workbook.createSheet("导入模板");
        //创建第1行
        Row row0 = sheet.createRow(0);
        row0.createCell(0).setCellValue("商品名称");
        row0.createCell(1).setCellValue("商品卖点");
        row0.createCell(2).setCellValue("商品分类");
        row0.createCell(3).setCellValue("运费模板");
        row0.createCell(4).setCellValue("计量单位");
        row0.createCell(5).setCellValue("发布状态");
        row0.createCell(6).setCellValue("商品图片");
        row0.createCell(7).setCellValue("成本价");
        row0.createCell(8).setCellValue("销售价");
        row0.createCell(9).setCellValue("库存");
        row0.createCell(10).setCellValue("重量");
        row0.createCell(11).setCellValue("货号");
        row0.createCell(12).setCellValue("详情");
        row0.createCell(13).setCellValue("规格项");
        row0.createCell(14).setCellValue("规格值");


        sheet.setColumnWidth(0, 7000);
        sheet.setColumnWidth(1, 7000);
        sheet.setColumnWidth(2, 7000);
        sheet.setColumnWidth(3, 7000);
        sheet.setColumnWidth(4, 7000);
        sheet.setColumnWidth(5, 3000);
        sheet.setColumnWidth(6, 7000);
        sheet.setColumnWidth(7, 3000);
        sheet.setColumnWidth(8, 3000);
        sheet.setColumnWidth(9, 3000);
        sheet.setColumnWidth(10, 3000);
        sheet.setColumnWidth(11, 7000);
        sheet.setColumnWidth(12, 7000);
        sheet.setColumnWidth(13, 3000);
        sheet.setColumnWidth(14, 3000);

        String goodsManagementCategory = storeDetailService.getStoreDetail(storeId).getGoodsManagementCategory();
        List<CategoryVO> categoryVOList = this.categoryService.getStoreCategory(goodsManagementCategory.split(","));
        List<String> categoryNameList = new ArrayList<>();

        //先简单写，后期优化
        //循环三次添加值
        //循环列表，存放ID-分类名称
        for (CategoryVO categoryVO1 : categoryVOList) {
            for (CategoryVO categoryVO2 : categoryVO1.getChildren()) {
                for (CategoryVO categoryVO3 : categoryVO2.getChildren()) {
                    categoryNameList.add(categoryVO3.getId() + "-" + categoryVO3.getName());
                }
            }
        }

        List<String> freightTemplateNameList = new ArrayList<>();
        //循环列表，存放ID-运费模板名称
        for (FreightTemplateVO freightTemplateVO : freightTemplateService.getFreightTemplateList(storeId)) {
            freightTemplateNameList.add(freightTemplateVO.getId() + "-" + freightTemplateVO.getName());
        }

        //获取计量单位
        List<String> goodsUnitList = new ArrayList<>();
        List<GoodsUnit> goodsUnitListVO = goodsUnitService.list();
        for (GoodsUnit goodsUnit : goodsUnitListVO) {
            goodsUnitList.add(goodsUnit.getId() + "-" + goodsUnit.getName());
        }

        //添加分类
        this.excelTo255(workbook, "hiddenCategoryVO", 1, categoryNameList.toArray(new String[]{}), 1, 5000, 2, 2);

        //添加运费模板
        this.excelTo255(workbook, "hiddenFreightTemplateVO", 2, freightTemplateNameList.toArray(new String[]{}), 1, 5000, 3, 3);

        //添加计量单位
        this.excelTo255(workbook, "hiddenGoodsUnit", 3, goodsUnitList.toArray(new String[]{}), 1, 5000, 4, 4);

        //添加计量单位
        this.excelTo255(workbook, "hiddenRelease", 4, new String[]{"上架", "仓库中"}, 1, 5000, 5, 5);


        ServletOutputStream out = null;
        try {
            //设置公共属性，列表名称
            response.setContentType("application/vnd.ms-excel;charset=utf-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + URLEncoder.encode("下载商品导入模板", "UTF8") + ".xls");
            out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
            log.error("下载商品导入模板错误", e);
        } finally {
            try {
                out.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void importExcel(MultipartFile files) throws Exception {
        InputStream inputStream;
        List<GoodsImportDTO> goodsImportDTOList = new ArrayList<>();

        inputStream = files.getInputStream();
        ExcelReader excelReader = ExcelUtil.getReader(inputStream);
        // 读取列表
        // 检测数据-查看分类、模板、计量单位是否存在
        List<List<Object>> read = excelReader.read(1, excelReader.getRowCount());
        for (List<Object> objects : read) {
            GoodsImportDTO goodsImportDTO = new GoodsImportDTO();

            String categoryId = objects.get(2).toString().substring(0, objects.get(2).toString().indexOf("-"));

            Category category = categoryService.getCategoryById(categoryId);
            if (category == null) {
                throw new ServiceException("商品分类不存在：" + objects.get(2).toString().substring(objects.get(2).toString().indexOf("-")));
            }

            String templateId = objects.get(3).toString().substring(0, objects.get(3).toString().indexOf("-"));
            FreightTemplateVO freightTemplateVO = freightTemplateService.getFreightTemplate(templateId);
            if (freightTemplateVO == null) {
                throw new ServiceException("配送模板不存在：" + objects.get(3).toString().substring(objects.get(3).toString().indexOf("-")));
            }

            goodsImportDTO.setGoodsName(objects.get(0).toString());
            goodsImportDTO.setSellingPoint(objects.get(1).toString());
            goodsImportDTO.setCategory(category);
            goodsImportDTO.setTemplate(templateId);
            goodsImportDTO.setGoodsUnit(objects.get(4).toString().substring(objects.get(4).toString().indexOf("-") + 1));
            goodsImportDTO.setRelease(objects.get(5).toString().equals("上架") ? true : false);

            List<Map<String, String>> images = new ArrayList<>();
            List<String> goodsGalleryList = new ArrayList<>();
            Map<String, String> map = new HashMap<>();
            map.put("url", objects.get(6).toString());
            images.add(map);
            goodsGalleryList.add(objects.get(6).toString());
            goodsImportDTO.setImages(images);
            goodsImportDTO.setGoodsGalleryList(goodsGalleryList);

            goodsImportDTO.setCost(Convert.toDouble(objects.get(7)));
            goodsImportDTO.setPrice(Convert.toDouble(objects.get(8)));
            goodsImportDTO.setQuantity(Convert.toInt(objects.get(9)));
            goodsImportDTO.setWeight(Convert.toDouble(objects.get(10)));
            goodsImportDTO.setSn(objects.get(11).toString());
            goodsImportDTO.setIntro("<p>" + objects.get(12).toString() + "</p>");
            goodsImportDTO.setSkuKey(objects.get(13).toString());
            goodsImportDTO.setSkuValue(objects.get(14).toString());
            goodsImportDTOList.add(goodsImportDTO);
        }
        //添加商品
        addGoodsList(goodsImportDTOList);

    }

    /**
     * 添加商品
     *
     * @param goodsImportDTOList
     */
    private void addGoodsList(List<GoodsImportDTO> goodsImportDTOList) {

        for (GoodsImportDTO goodsImportDTO : goodsImportDTOList) {
            GoodsOperationDTO goodsOperationDTO = new GoodsOperationDTO(goodsImportDTO);

            //获取父
            Category parentCategory = categoryService.getCategoryById(goodsImportDTO.getCategory().getParentId());
            goodsOperationDTO.setCategoryPath(parentCategory.getParentId() + "," + parentCategory.getId() + "," + goodsImportDTO.getCategory().getParentId());
            //添加商品
            goodsService.addGoods(goodsOperationDTO);
        }

    }

    /**
     * 表格
     *
     * @param workbook       表格
     * @param sheetName      sheet名称
     * @param sheetNameIndex 开始
     * @param sheetData      数据
     * @param firstRow       开始行
     * @param lastRow        结束行
     * @param firstCol       开始列
     * @param lastCol        结束列
     */
    private void excelTo255(Workbook workbook, String sheetName, int sheetNameIndex, String[] sheetData,
                            int firstRow, int lastRow, int firstCol, int lastCol) {
        //将下拉框数据放到新的sheet里，然后excle通过新的sheet数据加载下拉框数据
        Sheet hidden = workbook.createSheet(sheetName);

        //创建单元格对象
        Cell cell = null;
        //遍历我们上面的数组，将数据取出来放到新sheet的单元格中
        for (int i = 0, length = sheetData.length; i < length; i++) {
            //取出数组中的每个元素
            String name = sheetData[i];
            //根据i创建相应的行对象（说明我们将会把每个元素单独放一行）
            Row row = hidden.createRow(i);
            //创建每一行中的第一个单元格
            cell = row.createCell(0);
            //然后将数组中的元素赋值给这个单元格
            cell.setCellValue(name);
        }
        // 创建名称，可被其他单元格引用
        Name namedCell = workbook.createName();
        namedCell.setNameName(sheetName);
        // 设置名称引用的公式
        namedCell.setRefersToFormula(sheetName + "!$A$1:$A$" + sheetData.length);
        //加载数据,将名称为hidden的sheet中的数据转换为List形式
        DVConstraint constraint = DVConstraint.createFormulaListConstraint(sheetName);

        // 设置第一列的3-65534行为下拉列表
        // (3, 65534, 2, 2) ====> (起始行,结束行,起始列,结束列)
        CellRangeAddressList regions = new CellRangeAddressList(firstRow, lastRow, firstCol, lastCol);
        // 将设置下拉选的位置和数据的对应关系 绑定到一起
        DataValidation dataValidation = new HSSFDataValidation(regions, constraint);

        //将第二个sheet设置为隐藏
        workbook.setSheetHidden(sheetNameIndex, true);
        //将数据赋给下拉列表
        workbook.getSheetAt(0).addValidationData(dataValidation);
    }
}
