package cn.lili.modules.goods.support;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.modules.appointment.service.AppointmentService;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.enums.GoodsTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.CategoryService;
import cn.lili.modules.goods.service.GoodsGalleryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import lombok.NoArgsConstructor;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;

/**
 * 商品 SPU 视图对象组装器
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
    @Lazy
    private GoodsService goodsService;

    @Autowired
    private GoodsGalleryService goodsGalleryService;

    @Autowired
    @Lazy
    private GoodsSkuService goodsSkuService;

    @Autowired
    @Lazy
    private CategoryService categoryService;

    @Autowired
    @Lazy
    private AppointmentService appointmentService;

    @Autowired
    private Cache<GoodsVO> cache;

    /**
     * 全参构造函数，用于单元测试或非 Spring 环境灵活装配
     *
     * @param goodsMapper         商品数据映射器
     * @param goodsService        商品业务服务
     * @param goodsGalleryService 商品相册服务
     * @param goodsSkuService     SKU 业务服务
     * @param categoryService     分类服务
     * @param appointmentService 预约拓展服务
     * @param cache               缓存客户端
     */
    public GoodsVOAssembler(GoodsMapper goodsMapper,
                            GoodsService goodsService,
                            GoodsGalleryService goodsGalleryService,
                            GoodsSkuService goodsSkuService,
                            CategoryService categoryService,
                            AppointmentService appointmentService,
                            Cache<GoodsVO> cache) {
        this.goodsMapper = goodsMapper;
        this.goodsService = goodsService;
        this.goodsGalleryService = goodsGalleryService;
        this.goodsSkuService = goodsSkuService;
        this.categoryService = categoryService;
        this.appointmentService = appointmentService;
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

        Goods goods = this.goodsService != null ? this.goodsService.getById(goodsId)
                : (this.goodsMapper != null ? this.goodsMapper.selectById(goodsId) : null);
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

        if (goodsSkuService != null) {
            List<GoodsSkuVO> goodsListByGoodsId = goodsSkuService.getGoodsListByGoodsId(goodsId);
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

        if (GoodsTypeEnum.APPOINTMENT_GOODS.name().equals(goods.getGoodsType()) && appointmentService != null) {
            appointmentService.enrichGoodsVoForSeller(goodsVO);
        }

        if (cache != null) {
            cache.put(CachePrefix.GOODS.getPrefix() + goodsId, goodsVO, 7200L);
        }
        return goodsVO;
    }
}
