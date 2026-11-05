package cn.lili.controller.manager.member;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import cn.lili.modules.member.service.PaidMemberService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
/**
 * 平台付费会员配置与会员费记录。
 * @author Bulbasaur
 * @since 2026-11-04
 */
@RestController
@RequestMapping("/manager/member/paid")
public class PaidMemberManagerController {
    @Autowired private PaidMemberService service;
    @Autowired private cn.lili.modules.goods.service.GoodsSkuService skus;
    @Autowired private cn.lili.modules.store.service.StoreService stores;
    @Autowired private cn.lili.modules.goods.service.CategoryService categories;
    @Autowired private cn.lili.modules.goods.service.BrandService brands;
    @GetMapping("/config") public ResultMessage<?> config() { return ResultUtil.data(service.config()); }
    @PutMapping("/config") public ResultMessage<?> save(@RequestBody PaidMemberSetting config) { service.saveConfig(config); return ResultUtil.success(); }
    @GetMapping("/orders") public ResultMessage<?> orders(String memberId, String keyword, String status, PageVO page) { return ResultUtil.data(service.page(memberId, keyword, status, page)); }
    @GetMapping("/resources") public ResultMessage<?> resources(String type, @RequestParam java.util.List<String> ids) {
        if (ids.size() > 2000) throw new cn.lili.common.exception.ServiceException(cn.lili.common.enums.ResultCode.PARAMS_ERROR, "查询数量过多");
        return ResultUtil.data(ids.isEmpty() ? java.util.List.of() : "STORE".equals(type) ? stores.listByIds(ids) : skuResources(skus.listByIds(ids)));
    }
    @GetMapping("/discount-skus")
    public ResultMessage<?> discountSkus(String storeScope, @RequestParam(required=false) java.util.List<String> storeIds,
            cn.lili.modules.goods.entity.dto.GoodsSearchParams params) {
        var query = params.<cn.lili.modules.goods.entity.dos.GoodsSku>queryWrapper();
        if ("SELF".equals(storeScope)) {
            query.inSql("store_id", "SELECT id FROM li_store WHERE self_operated=1 AND delete_flag=0");
        } else if ("CUSTOM".equals(storeScope)) {
            if (storeIds == null || storeIds.isEmpty()) query.apply("1=0");
            else {
                if (storeIds.size() > 2000) throw new cn.lili.common.exception.ServiceException(cn.lili.common.enums.ResultCode.PARAMS_ERROR,"商家数量过多");
                query.in("store_id", storeIds);
            }
        } else throw new cn.lili.common.exception.ServiceException(cn.lili.common.enums.ResultCode.PARAMS_ERROR,"商家范围不正确");
        query.eq("sales_model","RETAIL").eq("goods_type","PHYSICAL_GOODS");
        return ResultUtil.data(skus.page(cn.lili.mybatis.util.PageUtil.initPage(params),query));
    }
    @PostMapping("/sku-import") public ResultMessage<?> importSkus(@RequestBody java.util.List<String> codes) {
        if (codes.isEmpty() || codes.size() > 200) throw new cn.lili.common.exception.ServiceException(cn.lili.common.enums.ResultCode.PARAMS_ERROR, "每次导入1～200个SKU编码");
        var found = skus.list(new com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper<cn.lili.modules.goods.entity.dos.GoodsSku>()
                .and(q -> q.in(cn.lili.modules.goods.entity.dos.GoodsSku::getId, codes).or().in(cn.lili.modules.goods.entity.dos.GoodsSku::getSn, codes)));
        var missing = codes.stream().filter(code -> found.stream().noneMatch(sku -> code.equals(sku.getId()) || code.equals(sku.getSn()))).toList();
        if (!missing.isEmpty()) throw new cn.lili.common.exception.ServiceException(cn.lili.common.enums.ResultCode.PARAMS_ERROR, "SKU不存在：" + String.join("、", missing));
        return ResultUtil.data(skuResources(found));
    }
    private java.util.List<java.util.Map<String,Object>> skuResources(java.util.List<cn.lili.modules.goods.entity.dos.GoodsSku> items) {
        var categoryIds = new java.util.HashSet<String>();
        var brandIds = new java.util.HashSet<String>();
        items.forEach(sku -> {
            if (sku.getCategoryPath() != null) java.util.Arrays.stream(sku.getCategoryPath().split(",")).map(String::trim).filter(id -> !id.isEmpty()).forEach(categoryIds::add);
            if (sku.getBrandId() != null && !sku.getBrandId().isBlank()) brandIds.add(sku.getBrandId());
        });
        var categoryNames = new java.util.HashMap<String,String>();
        var brandNames = new java.util.HashMap<String,String>();
        var storeIds = items.stream().map(cn.lili.modules.goods.entity.dos.GoodsSku::getStoreId).filter(java.util.Objects::nonNull).collect(java.util.stream.Collectors.toSet());
        var selfOperated = new java.util.HashMap<String,Boolean>();
        if (!storeIds.isEmpty()) stores.listByIds(storeIds).forEach(store -> selfOperated.put(store.getId(),Boolean.TRUE.equals(store.getSelfOperated())));
        if (!categoryIds.isEmpty()) categories.listByIds(categoryIds).forEach(category -> categoryNames.put(category.getId(), category.getName()));
        if (!brandIds.isEmpty()) brands.listByIds(brandIds).forEach(brand -> brandNames.put(brand.getId(), brand.getName()));
        return items.stream().map(sku -> {
            java.util.Map<String,Object> row = new java.util.LinkedHashMap<>();
            row.put("id",sku.getId()); row.put("sn",sku.getSn()); row.put("goodsName",sku.getGoodsName());
            row.put("simpleSpecs",sku.getSimpleSpecs()); row.put("price",sku.getPrice());
            row.put("storeId",sku.getStoreId()); row.put("storeName",sku.getStoreName());
            row.put("storeSelfOperated",selfOperated.getOrDefault(sku.getStoreId(),false));
            row.put("brandName",brandNames.getOrDefault(sku.getBrandId(),""));
            row.put("categoryName",sku.getCategoryPath() == null ? "" : java.util.Arrays.stream(sku.getCategoryPath().split(","))
                    .map(String::trim).map(categoryNames::get).filter(java.util.Objects::nonNull).collect(java.util.stream.Collectors.joining(" / ")));
            return row;
        }).toList();
    }
}
