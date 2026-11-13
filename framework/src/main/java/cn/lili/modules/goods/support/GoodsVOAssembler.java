package cn.lili.modules.goods.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.appointment.support.AppointmentGoodsMetadataSupport;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsGalleryService;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * 商品 SPU 视图对象组装器，商品实体统一通过Mapper查询，不反向依赖商品服务
 * <p>
 * 聚合 SPU 实体、相册图片、SKU 列表、分类路径、规格参数及预约拓展信息，统一维护商品详情视图及两小时缓存。
 *
 * @author Bulbasaur
 * @since 2026-11-12
 */
@Component
@NoArgsConstructor
public class GoodsVOAssembler {

    @Autowired
    private GoodsMapper goodsMapper;

    @Autowired
    private GoodsGalleryService goodsGalleryService;

    @Autowired
    private GoodsSkuReadSupport skuReads;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private AppointmentGoodsMetadataSupport appointmentGoodsMetadataSupport;

    @Autowired
    private Cache<GoodsVO> cache;

    /**
     * 全参构造函数，用于单元测试或非 Spring 环境灵活装配
     *
     * @param goodsMapper         商品数据映射器
     * @param goodsGalleryService 商品相册服务
     * @param skuReads             SKU基础查询组件
     * @param categoryService     分类服务
     * @param appointmentGoodsMetadataSupport 预约发品元数据组件
     * @param cache               缓存客户端
     */
    public GoodsVOAssembler(GoodsMapper goodsMapper,
                            GoodsGalleryService goodsGalleryService,
                            GoodsSkuReadSupport skuReads,
                            CategoryService categoryService,
                            AppointmentGoodsMetadataSupport appointmentGoodsMetadataSupport,
                            Cache<GoodsVO> cache) {
        this.goodsMapper = goodsMapper;
        this.goodsGalleryService = goodsGalleryService;
        this.skuReads = skuReads;
        this.categoryService = categoryService;
        this.appointmentGoodsMetadataSupport = appointmentGoodsMetadataSupport;
        this.cache = cache;
    }

    /**
     * 获取商品视图对象，优先读取缓存，未命中时组装并写入缓存
     *
     * @param goodsId 商品 ID
     * @return 完整的商品详情视图对象
     */
    public GoodsVO getGoodsVO(String goodsId) {
        GoodsVO goodsVO = cache != null ? cache.get(CachePrefix.GOODS.getPrefix() + goodsId) : null;
        if (goodsVO != null) {
            return goodsVO;
        }

        Goods goods = this.goodsMapper.selectById(goodsId);
        if (goods == null) {
            return null;
        }

        goodsVO = new GoodsVO();
        BeanUtils.copyProperties(goods, goodsVO);
        goodsVO.setId(goods.getId());

        List<String> images = new ArrayList<>();
        if (goodsGalleryService != null) {
            List<GoodsGallery> galleryList = goodsGalleryService.goodsGalleryList(goodsId);
            if (galleryList != null) {
                for (GoodsGallery goodsGallery : galleryList) {
                    images.add(goodsGallery.getOriginal());
                }
            }
        }
        goodsVO.setGoodsGalleryList(images);

        if (skuReads != null) {
            List<GoodsSkuVO> goodsListByGoodsId = skuReads.getGoodsListByGoodsId(goodsId);
            if (goodsListByGoodsId != null && !goodsListByGoodsId.isEmpty()) {
                goodsVO.setSkuList(goodsListByGoodsId);
            }
        }

        if (categoryService != null) {
            goodsVO.setCategoryName(categoryService.getCategoryNamesByPath(goods.getCategoryPath()));
        }

        if (CharSequenceUtil.isNotEmpty(goods.getParams())) {
            goodsVO.setGoodsParamsDTOList(GoodsSearchParamParser.parseAll(goods.getParams()));
        }

        if (GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goods.getGoodsType()) && appointmentGoodsMetadataSupport != null) {
            appointmentGoodsMetadataSupport.enrichGoodsVoForSeller(goodsVO);
        }

        if (cache != null) {
            cache.put(CachePrefix.GOODS.getPrefix() + goodsId, goodsVO, 7200L);
        }
        return goodsVO;
    }
}
