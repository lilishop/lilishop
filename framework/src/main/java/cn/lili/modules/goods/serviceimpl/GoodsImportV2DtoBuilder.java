package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.GoodsUnit;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.CategoryBrandVO;
import cn.lili.modules.goods.entity.vos.StoreGoodsLabelVO;
import cn.lili.modules.goods.service.CategoryBrandService;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsUnitService;
import cn.lili.modules.goods.service.StoreGoodsLabelService;
import cn.lili.modules.store.entity.vos.FreightTemplateVO;
import cn.lili.modules.store.service.FreightTemplateService;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * 商品批量导入v2 商品操作 DTO 组装支持组件
 * 负责将校验通过的商品组数据行映射为 GoodsOperationDTO，执行图片拆分、SKU 规格动态展开、最低售价试算、三级分类完整路径回溯及关联 ID 转换。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
public class GoodsImportV2DtoBuilder {

    /**
     * 将校验通过（已完成组内继承回填）的商品组组装为 GoodsOperationDTO。
     *
     * @param rows                   校验通过的商品组数据行（首行为商品级字段来源）
     * @param categoryService        分类服务
     * @param categoryBrandService   分类品牌服务
     * @param freightTemplateService 运费模板服务
     * @param goodsUnitService       计量单位服务
     * @param storeGoodsLabelService 店铺分类服务
     * @param storeId                当前商家店铺 ID
     * @return 可直接交给 goodsService.addGoods 的商品操作 DTO
     */
    public static GoodsOperationDTO buildGoodsOperationDTO(List<GoodsImportV2ServiceImpl.Row> rows,
                                                           CategoryService categoryService,
                                                           CategoryBrandService categoryBrandService,
                                                           FreightTemplateService freightTemplateService,
                                                           GoodsUnitService goodsUnitService,
                                                           StoreGoodsLabelService storeGoodsLabelService,
                                                           String storeId) {
        GoodsImportV2ServiceImpl.Row first = rows.get(0);
        List<String> gallery = splitImages(first.goodsImages);
        List<Map<String, Object>> skuList = new ArrayList<>(rows.size());
        double minPrice = Double.MAX_VALUE;
        for (GoodsImportV2ServiceImpl.Row row : rows) {
            Map<String, Object> sku = new LinkedHashMap<>();
            double price = Double.parseDouble(row.price);
            sku.put("price", price);
            sku.put("cost", Double.parseDouble(row.cost));
            sku.put("quantity", Integer.valueOf(row.quantity));
            sku.put("weight", Double.parseDouble(row.weight));
            sku.put("alertQuantity", row.alertQuantity.isEmpty() ? 0 : Integer.valueOf(row.alertQuantity));
            if (!row.spec1Name.isEmpty()) {
                sku.put(row.spec1Name, row.spec1Value);
            }
            if (!row.spec2Name.isEmpty()) {
                sku.put(row.spec2Name, row.spec2Value);
            }
            if (!row.spec3Name.isEmpty()) {
                sku.put(row.spec3Name, row.spec3Value);
            }
            sku.put("images", gallery);
            skuList.add(sku);
            minPrice = Math.min(minPrice, price);
        }
        GoodsOperationDTO dto = new GoodsOperationDTO();
        dto.setGoodsName(first.goodsName);
        dto.setPrice(minPrice);
        dto.setCategoryPath(resolveCategoryPath(first, categoryService));
        dto.setStoreCategoryPath(resolveStoreCategoryPath(first.storeCategory, storeGoodsLabelService, storeId));
        dto.setBrandId(resolveBrandId(first, categoryBrandService));
        dto.setTemplateId(resolveTemplateId(first.freightTemplate, freightTemplateService, storeId));
        dto.setGoodsUnit(resolveGoodsUnitName(first.goodsUnit, goodsUnitService));
        dto.setGoodsGalleryList(gallery);
        dto.setSkuList(skuList);
        dto.setIntro("");
        dto.setMobileIntro("");
        dto.setGoodsType(GoodsTypeEnum.PHYSICAL_GOODS.name());
        dto.setRelease(false);
        dto.setRecommend(false);
        dto.setGoodsParamsDTOList(null);
        return dto;
    }

    /**
     * 拆分商品图片列为 URL 列表（英文逗号分隔，逐项去空白）。
     *
     * @param goodsImages 图片列原始文本
     * @return 图片 URL 列表，首张为主图
     */
    public static List<String> splitImages(String goodsImages) {
        List<String> images = new ArrayList<>();
        for (String url : goodsImages.split(",")) {
            String image = url.trim();
            if (!image.isEmpty()) {
                images.add(image);
            }
        }
        return images;
    }

    /**
     * 回溯平台分类的三级分类路径（一级,二级,三级 ID 逗号串）。
     *
     * @param first           组内首行
     * @param categoryService 分类服务
     * @return 三级分类 ID 逗号串
     */
    public static String resolveCategoryPath(GoodsImportV2ServiceImpl.Row first, CategoryService categoryService) {
        Category leaf = categoryService.getCategoryById(GoodsImportV2Validator.categoryIdOf(first.categoryPath));
        Category second = categoryService.getCategoryById(leaf.getParentId());
        Category top = categoryService.getCategoryById(second.getParentId());
        return top.getId() + "," + second.getId() + "," + leaf.getId();
    }

    /**
     * 解析运费模板填值为本店运费模板 ID。
     *
     * @param freightTemplate        运费模板列填值
     * @param freightTemplateService 运费模板服务
     * @param storeId                当前店铺 ID
     * @return 运费模板 ID
     */
    public static String resolveTemplateId(String freightTemplate, FreightTemplateService freightTemplateService, String storeId) {
        FreightTemplateVO template = GoodsImportV2Validator.findOption(freightTemplate,
                freightTemplateService.getFreightTemplateList(storeId),
                FreightTemplateVO::getId, FreightTemplateVO::getName);
        return template == null ? freightTemplate : template.getId();
    }

    /**
     * 解析品牌填值为品牌 ID；未填品牌返回 null。
     *
     * @param first                组内首行
     * @param categoryBrandService 分类品牌服务
     * @return 品牌 ID，未填时返回 null
     */
    public static String resolveBrandId(GoodsImportV2ServiceImpl.Row first, CategoryBrandService categoryBrandService) {
        if (first.brand.isEmpty()) {
            return null;
        }
        CategoryBrandVO brand = GoodsImportV2Validator.findOption(first.brand,
                categoryBrandService.getCategoryBrandList(GoodsImportV2Validator.categoryIdOf(first.categoryPath)),
                CategoryBrandVO::getId, CategoryBrandVO::getName);
        return brand == null ? first.brand : brand.getId();
    }

    /**
     * 解析计量单位填值为单位名称。
     *
     * @param goodsUnit        计量单位列填值
     * @param goodsUnitService 计量单位服务
     * @return 计量单位名称
     */
    public static String resolveGoodsUnitName(String goodsUnit, GoodsUnitService goodsUnitService) {
        GoodsUnit unit = GoodsImportV2Validator.findOption(goodsUnit, goodsUnitService.list(), GoodsUnit::getId, GoodsUnit::getName);
        return unit == null ? goodsUnit : unit.getName();
    }

    /**
     * 解析店铺分类填值为本店店铺分类 ID 逗号串；未填返回 null。
     *
     * @param storeCategory          店铺分类列填值
     * @param storeGoodsLabelService 店铺分类服务
     * @param storeId                当前店铺 ID
     * @return 店铺分类 ID 逗号串，未填时返回 null
     */
    public static String resolveStoreCategoryPath(String storeCategory, StoreGoodsLabelService storeGoodsLabelService, String storeId) {
        if (storeCategory.isEmpty()) {
            return null;
        }
        List<StoreGoodsLabelVO> labels = storeGoodsLabelService.listByStoreId(storeId);
        List<String> ids = new ArrayList<>();
        for (String item : storeCategory.split(",")) {
            String label = item.trim();
            if (label.isEmpty()) {
                continue;
            }
            StoreGoodsLabelVO option = GoodsImportV2Validator.findOption(label, labels, StoreGoodsLabelVO::getId, StoreGoodsLabelVO::getLabelName);
            ids.add(option == null ? label : option.getId());
        }
        return ids.isEmpty() ? null : String.join(",", ids);
    }
}
