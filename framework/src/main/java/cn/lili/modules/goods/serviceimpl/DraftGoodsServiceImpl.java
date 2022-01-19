package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.json.JSONArray;
import cn.hutool.json.JSONUtil;
import cn.lili.modules.goods.entity.dos.Category;
import cn.lili.modules.goods.entity.dos.DraftGoods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.modules.goods.entity.dto.DraftGoodsDTO;
import cn.lili.modules.goods.entity.dto.DraftGoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsParamsDTO;
import cn.lili.modules.goods.entity.vos.DraftGoodsVO;
import cn.lili.modules.goods.mapper.DraftGoodsMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.DraftGoodsService;
import cn.lili.modules.goods.service.GoodsGalleryService;
import cn.lili.modules.goods.service.GoodsSkuService;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;

/**
 * 草稿商品业务层实现
 *
 * @author paulG
 * @since 2020/12/19
 **/
@Service
public class DraftGoodsServiceImpl extends ServiceImpl<DraftGoodsMapper, DraftGoods> implements DraftGoodsService {
    /**
     * 分类
     */
    @Autowired
    private CategoryService categoryService;
    /**
     * 商品相册
     */
    @Autowired
    private GoodsGalleryService goodsGalleryService;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public boolean addGoodsDraft(DraftGoodsDTO draftGoods) {
        draftGoods.setGoodsGalleryListJson(JSONUtil.toJsonStr(draftGoods.getGoodsGalleryList()));
        draftGoods.setSkuListJson(JSONUtil.toJsonStr(draftGoods.getSkuList()));
        draftGoods.setGoodsParamsListJson(JSONUtil.toJsonStr(draftGoods.getGoodsParamsDTOList()));
        return this.save(draftGoods);
    }

    @Override
    public boolean updateGoodsDraft(DraftGoodsDTO draftGoods) {
        draftGoods.setGoodsGalleryListJson(JSONUtil.toJsonStr(draftGoods.getGoodsGalleryList()));
        draftGoods.setSkuListJson(JSONUtil.toJsonStr(draftGoods.getSkuList()));
        draftGoods.setGoodsParamsListJson(JSONUtil.toJsonStr(draftGoods.getGoodsParamsDTOList()));
        return this.updateById(draftGoods);
    }

    @Override
    public void saveGoodsDraft(DraftGoodsDTO draftGoods) {

        if (draftGoods.getGoodsGalleryList() != null && !draftGoods.getGoodsGalleryList().isEmpty()) {
            GoodsGallery goodsGallery = goodsGalleryService.getGoodsGallery(draftGoods.getGoodsGalleryList().get(0));
            draftGoods.setOriginal(goodsGallery.getOriginal());
            draftGoods.setSmall(goodsGallery.getSmall());
            draftGoods.setThumbnail(goodsGallery.getThumbnail());
        }
        draftGoods.setGoodsGalleryListJson(JSONUtil.toJsonStr(draftGoods.getGoodsGalleryList()));
        draftGoods.setSkuListJson(JSONUtil.toJsonStr(this.getGoodsSkuList(draftGoods.getSkuList())));
        draftGoods.setGoodsParamsListJson(JSONUtil.toJsonStr(draftGoods.getGoodsParamsDTOList()));
        this.saveOrUpdate(draftGoods);
    }

    @Override
    public void deleteGoodsDraft(String id) {
        this.removeById(id);
    }

    @Override
    public DraftGoodsVO getDraftGoods(String id) {

        DraftGoods draftGoods = this.getById(id);
        DraftGoodsVO draftGoodsVO = new DraftGoodsVO();
        BeanUtil.copyProperties(draftGoods, draftGoodsVO);
        //商品分类名称赋值
        List<String> categoryName = new ArrayList<>();
        String[] strArray = draftGoods.getCategoryPath().split(",");
        List<Category> categories = categoryService.listByIds(Arrays.asList(strArray));
        for (Category category : categories) {
            categoryName.add(category.getName());
        }
        draftGoodsVO.setCategoryName(categoryName);
        draftGoodsVO.setGoodsParamsDTOList(JSONUtil.toList(JSONUtil.parseArray(draftGoods.getGoodsParamsListJson()), GoodsParamsDTO.class));
        draftGoodsVO.setGoodsGalleryList(JSONUtil.toList(JSONUtil.parseArray(draftGoods.getGoodsGalleryListJson()), String.class));
        JSONArray jsonArray = JSONUtil.parseArray(draftGoods.getSkuListJson());
        List<GoodsSku> list = JSONUtil.toList(jsonArray, GoodsSku.class);
        draftGoodsVO.setSkuList(goodsSkuService.getGoodsSkuVOList(list));
        return draftGoodsVO;
    }

    @Override
    public IPage<DraftGoods> getDraftGoods(DraftGoodsSearchParams searchParams) {
        return this.page(PageUtil.initPage(searchParams), searchParams.queryWrapper());
    }

    /**
     * 获取sku集合
     *
     * @param skuList sku列表
     * @return sku集合
     */
    private List<GoodsSku> getGoodsSkuList(List<Map<String, Object>> skuList) {
        List<GoodsSku> skus = new ArrayList<>();
        for (Map<String, Object> skuVO : skuList) {
            GoodsSku add = this.add(skuVO);
            skus.add(add);
        }
        return skus;
    }

    private GoodsSku add(Map<String, Object> map) {
        Map<String, Object> specMap = new HashMap<>(2);
        GoodsSku sku = new GoodsSku();
        for (Map.Entry<String, Object> m : map.entrySet()) {
            switch (m.getKey()) {
                case "sn":
                    sku.setSn(m.getValue() != null ? m.getValue().toString() : "");
                    break;
                case "cost":
                    sku.setCost(Convert.toDouble(m.getValue()));
                    break;
                case "price":
                    sku.setPrice(Convert.toDouble(m.getValue()));
                    break;
                case "quantity":
                    sku.setQuantity(Convert.toInt(m.getValue()));
                    break;
                case "weight":
                    sku.setWeight(Convert.toDouble(m.getValue()));
                    break;
                default:
                    specMap.put(m.getKey(), m.getValue());
                    break;
            }
        }
        sku.setSpecs(JSONUtil.toJsonStr(specMap));
        return sku;
    }

}
