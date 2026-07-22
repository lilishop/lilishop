package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.NumberUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.event.TransactionCommitSendMQEvent;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.RocketmqCustomProperties;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsGallery;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.entity.dto.GoodsOperationDTO;
import cn.lili.modules.goods.entity.dto.GoodsSearchParams;
import cn.lili.modules.goods.entity.dto.GoodsSkuDTO;
import cn.lili.modules.goods.entity.dto.GoodsSkuStockDTO;
import cn.lili.modules.goods.entity.enums.GoodsAuthEnum;
import cn.lili.modules.goods.entity.enums.GoodsSalesModeEnum;
import cn.lili.modules.goods.entity.enums.GoodsStatusEnum;
import cn.lili.modules.goods.entity.enums.GoodsStockTypeEnum;
import cn.lili.modules.goods.entity.vos.GoodsSkuSpecVO;
import cn.lili.modules.goods.entity.vos.GoodsSkuVO;
import cn.lili.modules.goods.entity.vos.GoodsVO;
import cn.lili.modules.goods.entity.vos.SpecValueVO;
import cn.lili.modules.goods.mapper.GoodsSkuMapper;
import cn.lili.modules.goods.service.GoodsGalleryService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.goods.service.WholesaleService;
import cn.lili.modules.goods.sku.GoodsSkuBuilder;
import cn.lili.modules.goods.sku.render.SalesModelRender;
import cn.lili.modules.member.entity.dos.FootPrint;
import cn.lili.modules.promotion.entity.dos.Coupon;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.enums.CouponGetEnum;
import cn.lili.modules.promotion.service.CouponService;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.search.entity.dos.GoodsIndex;
import cn.lili.modules.search.service.GoodsIndexService;
import cn.lili.mybatis.BaseEntity;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.message.MessageSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.alibaba.fastjson2.JSON;
import com.alibaba.fastjson2.JSONArray;
import com.alibaba.fastjson2.JSONObject;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import cn.lili.message.LiliMessageTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.util.*;
import java.util.stream.Collectors;

/**
 * ??sku?????
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Service
public class GoodsSkuServiceImpl extends ServiceImpl<GoodsSkuMapper, GoodsSku> implements GoodsSkuService {

    /**
     * ??
     */
    @Autowired
    private Cache cache;
    /**
     * ??
     */
    @Autowired
    @Lazy
    private MemberCouponService memberCouponService;
    /**
     * ????
     */
    @Autowired
    private GoodsGalleryService goodsGalleryService;
    /**
     * rocketMq
     */
    @Autowired
    private LiliMessageTemplate liliMessageTemplate;
    /**
     * rocketMq??
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * ??
     */
    @Autowired
    @Lazy
    private GoodsService goodsService;
    /**
     * ????
     */
    @Autowired
    @Lazy
    private GoodsIndexService goodsIndexService;

    @Autowired
    @Lazy
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private WholesaleService wholesaleService;

    @Autowired
    @Lazy
    private CouponService couponService;

    @Autowired
    private List<SalesModelRender> salesModelRenders;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void add(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        // ??????
        if (goodsOperationDTO.getSkuList() == null || goodsOperationDTO.getSkuList().isEmpty()) {
            throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU);
        }
        // ??????????
        List<GoodsSku> goodsSkus = GoodsSkuBuilder.buildBatch(goods, goodsOperationDTO.getSkuList());
        renderGoodsSkuList(goodsSkus, goodsOperationDTO);

        if (!goodsSkus.isEmpty()) {
            this.saveOrUpdateBatch(goodsSkus);
            this.updateGoodsStock(goodsSkus);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        // ??????
        if (goodsOperationDTO.getSkuList() == null || goodsOperationDTO.getSkuList().isEmpty()) {
            throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU);
        }
        List<GoodsSku> skuList;
        //????sku??
        if (Boolean.TRUE.equals(goodsOperationDTO.getRegeneratorSkuFlag())) {
            skuList = GoodsSkuBuilder.buildBatch(goods, goodsOperationDTO.getSkuList());
            renderGoodsSkuList(skuList, goodsOperationDTO);
            List<GoodsSkuVO> goodsListByGoodsId = getGoodsListByGoodsId(goods.getId());
            List<String> oldSkuIds = new ArrayList<>();
            //?????
            for (GoodsSkuVO goodsSkuVO : goodsListByGoodsId) {
                oldSkuIds.add(goodsSkuVO.getId());
                cache.remove(GoodsSkuService.getCacheKeys(goodsSkuVO.getId()));
            }

            //??sku??
            goodsGalleryService.removeByGoodsId(goods.getId());

            //??mq??
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.SKU_DELETE.name();
            asyncSendIfPresent(destination, JSON.toJSONString(oldSkuIds));
        } else {
            skuList = new ArrayList<>();
            for (Map<String, Object> map : goodsOperationDTO.getSkuList()) {
                GoodsSku sku = GoodsSkuBuilder.build(goods, map);
                renderGoodsSku(sku, goodsOperationDTO);
                skuList.add(sku);
                this.clearCache(sku.getId());
            }
        }
        if (!skuList.isEmpty()) {
            LambdaQueryWrapper<GoodsSku> unnecessarySkuIdsQuery = new LambdaQueryWrapper<>();
            unnecessarySkuIdsQuery.eq(GoodsSku::getGoodsId, goods.getId());
            unnecessarySkuIdsQuery.notIn(GoodsSku::getId,
                    skuList.stream().map(BaseEntity::getId).collect(Collectors.toList()));
            this.remove(unnecessarySkuIdsQuery);
            this.saveOrUpdateBatch(skuList);
            this.updateGoodsStock(skuList);
        }
    }

    /**
     * ????sku
     *
     * @param goodsSku sku??
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(GoodsSku goodsSku) {
        this.updateById(goodsSku);
        cache.remove(GoodsSkuService.getCacheKeys(goodsSku.getId()));
        cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
    }


    /**
     * ??sku??
     *
     * @param skuId skuID
     */
    @Override
    public void clearCache(String skuId) {
        cache.remove(GoodsSkuService.getCacheKeys(skuId));
    }

    @Override
    public GoodsSku getGoodsSkuByIdFromCache(String id) {
        //??????sku
        GoodsSku goodsSku = (GoodsSku) cache.get(GoodsSkuService.getCacheKeys(id));
        //???????????????????????
        if (goodsSku == null) {
            goodsSku = this.getById(id);
            if (goodsSku == null) {
                return null;
            }
            cache.put(GoodsSkuService.getCacheKeys(id), goodsSku,72 * 60 * 60L);
        }

        //??????
        Integer integer = (Integer) cache.get(GoodsSkuService.getStockCacheKey(id));

        //?????,?????????
        if (integer != null && !goodsSku.getQuantity().equals(integer)) {
            //?????????
            goodsSku.setQuantity(integer);
            cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
        }
        return goodsSku;
    }

    @Override
    public GoodsSku getCanPromotionGoodsSkuByIdFromCache(String skuId) {
        GoodsSku goodsSku = this.getGoodsSkuByIdFromCache(skuId);
        if (goodsSku != null && GoodsSalesModeEnum.WHOLESALE.name().equals(goodsSku.getSalesModel())) {
            throw new ServiceException(ResultCode.PROMOTION_GOODS_DO_NOT_JOIN_WHOLESALE, goodsSku.getGoodsName());
        }
        return goodsSku;
    }

    @Override
    public Map<String, Object> getGoodsSkuDetail(String goodsId, String skuId) {
        Map<String, Object> map = new HashMap<>(16);
        //????VO
        GoodsVO goodsVO = goodsService.getGoodsVO(goodsId);
        //??skuid????????VO?sku????
        if (CharSequenceUtil.isEmpty(skuId) || "undefined".equals(skuId)) {
            skuId = goodsVO.getSkuList().get(0).getId();
        }
        //??????Sku
        GoodsSku goodsSku = this.getGoodsSkuByIdFromCache(skuId);
        //??????ID????SKU?????
        if (goodsVO == null || goodsSku == null) {
            //??mq??
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_DELETE.name();
            asyncSendIfPresent(destination, JSON.toJSONString(Collections.singletonList(goodsId)));
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        //????||???????||??????????????
        if (GoodsStatusEnum.DOWN.name().equals(goodsVO.getMarketEnable()) || !GoodsAuthEnum.PASS.name().equals(goodsVO.getAuthFlag()) || Boolean.TRUE.equals(goodsVO.getDeleteFlag())) {
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_DELETE.name();
            asyncSendIfPresent(destination, JSON.toJSONString(Collections.singletonList(goodsId)));
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        //???????????
        GoodsIndex goodsIndex = goodsIndexService.findById(skuId);
        if (goodsIndex == null) {
            goodsIndex = goodsIndexService.buildGoodsIndex(goodsSku);
        }

        //????
        GoodsSkuVO goodsSkuDetail = this.getGoodsSkuVO(goodsSku);

        Map<String, Object> promotionMap = goodsIndex == null ? Collections.emptyMap() : goodsIndex.getPromotionMap();
        AuthUser currentUser = UserContext.getCurrentUser();
        //???????????
        if (promotionMap != null && !promotionMap.isEmpty()) {
            promotionMap = promotionMap.entrySet().stream().parallel().filter(i -> {
                JSONObject jsonObject = JSON.parseObject(String.valueOf(i.getValue()));
                if (i.getKey().contains(PromotionTypeEnum.COUPON.name()) && currentUser != null) {
                    Integer couponLimitNum = jsonObject.getInteger("couponLimitNum");
                    Coupon coupon = couponService.getById(jsonObject.getString("id"));
                    if (coupon == null || (coupon.getPublishNum() != 0 && coupon.getReceivedNum() >= coupon.getPublishNum())) {
                        return false;
                    }
                    if (couponLimitNum > 0) {
                        Long count = memberCouponService.getMemberCouponNum(currentUser.getId(), jsonObject.getString("id"));
                        if (count >= couponLimitNum) {
                            return false;
                        }
                    }
                }
                // ?????????????????
                return (jsonObject.get("getType") == null || CouponGetEnum.FREE.name().equals(jsonObject.getString("getType"))) && (jsonObject.get("startTime") != null && jsonObject.getObject("startTime", Date.class).getTime() <= System.currentTimeMillis()) && (jsonObject.get("endTime") == null || jsonObject.getObject("endTime", Date.class).getTime() >= System.currentTimeMillis());
            }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

            Optional<Map.Entry<String, Object>> containsPromotion =
                    promotionMap.entrySet().stream().filter(i -> i.getKey().contains(PromotionTypeEnum.SECKILL.name()) || i.getKey().contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
            if (containsPromotion.isPresent()) {
                JSONObject jsonObject = JSON.parseObject(String.valueOf(containsPromotion.get().getValue()));
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setSkuId(skuId);
                searchParams.setPromotionId(jsonObject.getString("id"));
                PromotionGoods promotionsGoods = promotionGoodsService.getPromotionsGoods(searchParams);
                if (promotionsGoods != null && promotionsGoods.getPrice() != null) {
                    goodsSkuDetail.setPromotionFlag(true);
                    goodsSkuDetail.setPromotionPrice(promotionsGoods.getPrice());
                }
            } else {
                goodsSkuDetail.setPromotionFlag(false);
                goodsSkuDetail.setPromotionPrice(null);
            }

        }
        if (goodsSkuDetail.getGoodsGalleryList() == null || goodsSkuDetail.getGoodsGalleryList().isEmpty()) {
            goodsSkuDetail.setGoodsGalleryList(goodsVO.getGoodsGalleryList());
        } else {
            goodsSkuDetail.getGoodsGalleryList().addAll(goodsVO.getGoodsGalleryList());
        }
        map.put("data", goodsSkuDetail);

        //????
        map.put("wholesaleList", GoodsSalesModeEnum.WHOLESALE.name().equals(goodsVO.getSalesModel()) ?
                wholesaleService.findByGoodsId(goodsSkuDetail.getGoodsId()) : Collections.emptyList());
        map.put("categoryName", CharSequenceUtil.isNotEmpty(goodsIndex.getCategoryNamePath()) ?
                goodsIndex.getCategoryNamePath().split(",") : null);

        //??????
        map.put("specs", this.groupBySkuAndSpec(goodsVO.getSkuList()));
        map.put("promotionMap", promotionMap);

        //??????
        if (goodsVO.getGoodsParamsDTOList() != null && !goodsVO.getGoodsParamsDTOList().isEmpty()) {
            map.put("goodsParamsDTOList", goodsVO.getGoodsParamsDTOList());
        }

        //??????
        if (currentUser != null) {
            FootPrint footPrint = new FootPrint(currentUser.getId(), goodsIndex.getStoreId(), goodsId, skuId);
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.VIEW_GOODS.name();
            asyncSendIfPresent(destination, footPrint);
        }
        return map;
    }

    /**
     * ????sku??
     *
     * @param goods ????(Id,MarketEnable/AuthFlag)
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatus(Goods goods) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(CharSequenceUtil.isNotEmpty(goods.getId()), GoodsSku::getGoodsId, goods.getId());
        updateWrapper.eq(CharSequenceUtil.isNotEmpty(goods.getStoreId()), GoodsSku::getStoreId, goods.getStoreId());
        updateWrapper.set(GoodsSku::getMarketEnable, goods.getMarketEnable());
        updateWrapper.set(GoodsSku::getAuthFlag, goods.getAuthFlag());
        updateWrapper.set(GoodsSku::getDeleteFlag, goods.getDeleteFlag());
        boolean update = this.update(updateWrapper);
        if (Boolean.TRUE.equals(update)) {
            List<GoodsSku> goodsSkus = this.getGoodsSkuListByGoodsId(goods.getId());
            for (GoodsSku sku : goodsSkus) {
                cache.remove(GoodsSkuService.getCacheKeys(sku.getId()));
                if (GoodsStatusEnum.UPPER.name().equals(goods.getMarketEnable()) && GoodsAuthEnum.PASS.name().equals(goods.getAuthFlag())) {
                    cache.put(GoodsSkuService.getCacheKeys(sku.getId()), sku);
                }
            }
        }
    }

    /**
     * ????id????sku??
     *
     * @param storeId      ??id
     * @param marketEnable ????
     * @param authFlag     ????
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatusByStoreId(String storeId, String marketEnable, String authFlag) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getStoreId, storeId);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(marketEnable), GoodsSku::getMarketEnable, marketEnable);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(authFlag), GoodsSku::getAuthFlag, authFlag);
        boolean update = this.update(updateWrapper);
        if (Boolean.TRUE.equals(update) && GoodsStatusEnum.DOWN.name().equals(marketEnable)) {
            cache.vagueDel(CachePrefix.GOODS_SKU.getPrefix());
        }
    }

    @Override
    public List<GoodsSku> getGoodsSkuByIdFromCache(List<String> ids) {
        List<String> keys = new ArrayList<>();
        for (String id : ids) {
            keys.add(GoodsSkuService.getCacheKeys(id));
        }
        List<GoodsSku> list = cache.multiGet(keys);
        if (list == null || list.isEmpty()) {
            list = new ArrayList<>();
            List<GoodsSku> goodsSkus = listByIds(ids);
            for (GoodsSku skus : goodsSkus) {
                cache.put(GoodsSkuService.getCacheKeys(skus.getId()), skus);
                list.add(skus);
            }
        }
        return list;
    }

    @Override
    public List<GoodsSkuVO> getGoodsListByGoodsId(String goodsId) {
        List<GoodsSku> list = this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId));
        return this.getGoodsSkuVOList(list);
    }

    /**
     * ??goodsId????goodsSku
     *
     * @param goodsId ??id
     * @return goodsSku??
     */
    @Override
    public List<GoodsSku> getGoodsSkuListByGoodsId(String goodsId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId));
    }

    @Override
    public List<GoodsSku> listByIdsAndStoreId(List<String> skuIds, String storeId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>()
                .in(GoodsSku::getId, skuIds)
                .eq(GoodsSku::getStoreId, storeId));
    }

    @Override
    public List<GoodsSku> listByGoodsIdAndStoreId(String goodsId, String storeId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>()
                .eq(GoodsSku::getGoodsId, goodsId)
                .eq(GoodsSku::getStoreId, storeId));
    }

    @Override
    public List<GoodsSkuVO> getGoodsSkuVOList(List<GoodsSku> list) {
        List<GoodsSkuVO> goodsSkuVOS = new ArrayList<>();
        for (GoodsSku goodsSku : list) {
            GoodsSkuVO goodsSkuVO = this.getGoodsSkuVO(goodsSku);
            goodsSkuVOS.add(goodsSkuVO);
        }
        return goodsSkuVOS;
    }

    @Override
    public GoodsSkuVO getGoodsSkuVO(GoodsSku goodsSku) {
        //?????
        GoodsSkuVO goodsSkuVO = new GoodsSkuVO(goodsSku);
        //??sku??
        JSONObject jsonObject = JSON.parseObject(goodsSku.getSpecs());
        //????sku??
        List<SpecValueVO> specValueVOS = new ArrayList<>();
        //????sku??
        List<String> goodsGalleryList = new ArrayList<>();
        //?????sku??
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            SpecValueVO specValueVO = new SpecValueVO();
            if ("images".equals(entry.getKey())) {
                specValueVO.setSpecName(entry.getKey());
                JSONArray jsonArray = (JSONArray) entry.getValue();
                List<String> specImages = jsonArray.toJavaList(String.class);
                specValueVO.setSpecImage(specImages);
                goodsGalleryList = new ArrayList<>(specImages);
            } else {
                specValueVO.setSpecName(entry.getKey());
                specValueVO.setSpecValue(entry.getValue().toString());
            }
            specValueVOS.add(specValueVO);
        }
        goodsSkuVO.setGoodsGalleryList(goodsGalleryList);
        goodsSkuVO.setSpecList(specValueVOS);
        return goodsSkuVO;

    }

    @Override
    public IPage<GoodsSku> getGoodsSkuByPage(GoodsSearchParams searchParams) {
        return this.page(PageUtil.initPage(searchParams), searchParams.queryWrapper());
    }

    @Override
    public void queryExportStock(HttpServletResponse response, GoodsSearchParams searchParams) {
        List<GoodsSkuStockDTO> goodsSkuStockDTOList = this.baseMapper.queryStocks(searchParams.queryWrapper());
        XSSFWorkbook workbook = initStockExportData(goodsSkuStockDTOList);
        try {
            // ?????
            String fileName = URLEncoder.encode("????", "UTF-8");
            response.setContentType("application/vnd.ms-excel;charset=UTF-8");
            response.setHeader("Content-Disposition", "attachment;filename=" + fileName + ".xlsx");

            ServletOutputStream out = response.getOutputStream();
            workbook.write(out);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                workbook.close();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public void importStock(String storeId, MultipartFile file) {
        List<GoodsSkuStockDTO> goodsSkuStockDTOList = new ArrayList<>();
        try (InputStream inputStream = file.getInputStream()) {
            // ?? WorkbookFactory.create ???? Excel ??
            Workbook workbook = WorkbookFactory.create(inputStream);
            // ????????sheet
            Sheet sheet = workbook.getSheetAt(0);

            // ?????sheet???????10002?
            if (sheet.getPhysicalNumberOfRows() > 10002) {
                throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "Excel????10002?");
            }
            // ???????
            Iterator<Row> rowIterator = sheet.rowIterator();
            int rowIndex = 0;
            while (rowIterator.hasNext()) {
                Row row = rowIterator.next();
                rowIndex++;

                // ????
                if (rowIndex < 3) {
                    continue;
                }

                List<Object> objects = new ArrayList<>();
                for (int i = 0; i < 4; i++) {
                    objects.add(getCellValue(row.getCell(i)));
                }
                log.error(getCellValue(row.getCell(2)));
                log.error(getCellValue(row.getCell(3)));
                // ??????
                if (!"?".equals(getCellValue(row.getCell(2))) && !"?".equals(getCellValue(row.getCell(2)))) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "???????????????????");
                } else if (!NumberUtil.isInteger(getCellValue(row.getCell(3))) || Integer.parseInt(getCellValue(row.getCell(3))) < 0) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "????????");
                } else if (this.count(new LambdaQueryWrapper<GoodsSku>()
                        .eq(GoodsSku::getGoodsId, getCellValue(row.getCell(0)))
                        .eq(GoodsSku::getId, getCellValue(row.getCell(1)))
                        .eq(GoodsSku::getStoreId, storeId)) == 0) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "?" + rowIndex + "??????");
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

        // ????????
        this.updateStocksByType(goodsSkuStockDTOList);
    }

    private String getCellValue(Cell cell) {
        if (cell == null) {
            return "";
        }
        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (DateUtil.isCellDateFormatted(cell)) {
                    return cell.getDateCellValue().toString();
                } else {
                    // ??????????????
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


    @Override
    public IPage<GoodsSkuDTO> getGoodsSkuDTOByPage(Page<GoodsSkuDTO> page, Wrapper<GoodsSkuDTO> queryWrapper) {
        IPage<GoodsSkuDTO> result = this.baseMapper.queryByParams(page, queryWrapper);
        if (result != null && result.getRecords() != null) {
            result.getRecords().forEach(this::clearBrandInfoWhenNoBrand);
        }
        return result;
    }

    private void clearBrandInfoWhenNoBrand(GoodsSkuDTO sku) {
        if (sku != null && "0".equals(sku.getBrandId())) {
            sku.setBrandName(null);
            sku.setBrandLogo(null);
        }
    }

    /**
     * ??????sku??
     *
     * @param searchParams ????
     * @return ??sku??
     */
    @Override
    public List<GoodsSku> getGoodsSkuByList(GoodsSearchParams searchParams) {
        return this.list(searchParams.queryWrapper());
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStocks(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        List<String> skuIds = goodsSkuStockDTOS.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> goodsSkuStockList = this.baseMapper.queryStocks(GoodsSearchParams.builder().ids(skuIds).build().queryWrapper());
        Map<String, List<GoodsSkuStockDTO>> groupByGoodsIds = goodsSkuStockList.stream().collect(Collectors.groupingBy(GoodsSkuStockDTO::getGoodsId));

        //?????????
        for (Map.Entry<String, List<GoodsSkuStockDTO>> entry : groupByGoodsIds.entrySet()) {
            //??
            for (GoodsSkuStockDTO goodsSku : entry.getValue()) {
                goodsSkuStockDTOS.stream().filter(i -> i.getSkuId().equals(goodsSku.getSkuId())).findFirst().ifPresent(i -> goodsSku.setQuantity(i.getQuantity()));

                this.updateStock(goodsSku.getSkuId(), goodsSku.getQuantity());
            }
            //????????
            goodsService.updateStock(entry.getKey());
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStocksByType(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {

        // ?????goodsID???????goodsID
        List<String> goodsIds = goodsSkuStockDTOS.stream()
                .map(GoodsSkuStockDTO::getGoodsId)
                .distinct()
                .collect(Collectors.toList());

        //??SKU??
        for (GoodsSkuStockDTO goodsSkuStockDTO : goodsSkuStockDTOS) {
            this.updateStock(goodsSkuStockDTO.getSkuId(), goodsSkuStockDTO.getQuantity(), goodsSkuStockDTO.getType());
        }
        //??SPU??
        for (String goodsId : goodsIds) {
            goodsService.updateStock(goodsId);
        }
    }


    @Override
    public void updateAlertQuantity(GoodsSkuStockDTO goodsSkuStockDTO) {
        GoodsSku goodsSku = this.getById(goodsSkuStockDTO.getSkuId());
        goodsSku.setAlertQuantity(goodsSkuStockDTO.getAlertQuantity());
        //??????????????????????????
        cache.remove(CachePrefix.GOODS.getPrefix() + goodsSku.getGoodsId());
        this.update(goodsSku);
    }

    @Override
    public void batchUpdateAlertQuantity(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {
        List<GoodsSku> goodsSkuList = new ArrayList<>();
        List<String> skuIds = goodsSkuStockDTOS.stream().map(GoodsSkuStockDTO::getSkuId).collect(Collectors.toList());
        List<GoodsSkuStockDTO> goodsSkuStockList = this.baseMapper.queryStocks(GoodsSearchParams.builder().ids(skuIds).build().queryWrapper());
        List<String> goodsIdList = goodsSkuStockList.stream().map(GoodsSkuStockDTO::getGoodsId).collect(Collectors.toList());
        HashSet<String> uniqueSet = new HashSet<>(goodsIdList);
        // ???????????
        List<String> uniqueGoodsIdList = new ArrayList<>(uniqueSet);
        for (String goodsId : uniqueGoodsIdList) {
            cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
        }
        //??????
        for (GoodsSkuStockDTO goodsSkuStockDTO : goodsSkuStockDTOS) {
            GoodsSku goodsSku = this.getById(goodsSkuStockDTO.getSkuId());
            goodsSku.setAlertQuantity(goodsSkuStockDTO.getAlertQuantity());
            goodsSkuList.add(goodsSku);
        }
        this.updateBatchById(goodsSkuList);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity) {
        GoodsSku goodsSku = getGoodsSkuByIdFromCache(skuId);
        if (goodsSku != null) {
            //????sku??????(???????0?  ???????),????????? ???????
            boolean isFlag = goodsSku.getQuantity() <= 0;

            goodsSku.setQuantity(quantity);
            boolean update = this.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, skuId).set(GoodsSku::getQuantity, quantity));
            if (update) {
                cache.remove(CachePrefix.GOODS.getPrefix() + goodsSku.getGoodsId());
            }
            cache.put(GoodsSkuService.getCacheKeys(skuId), goodsSku);
            cache.put(GoodsSkuService.getStockCacheKey(skuId), quantity);

            this.promotionGoodsService.updatePromotionGoodsStock(goodsSku.getId(), quantity);
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, String type) {
        GoodsSku goodsSku = getGoodsSkuByIdFromCache(skuId);
        if (goodsSku != null) {
            //????sku??????(???????0?  ???????),????????? ???????
            boolean isFlag = goodsSku.getQuantity() <= 0;

            //??????
            if (type.equals(GoodsStockTypeEnum.ADD.name())) {
                quantity = Convert.toInt(NumberUtil.add(goodsSku.getQuantity(), quantity));
            } else {
                quantity = Convert.toInt(NumberUtil.sub(goodsSku.getQuantity(), quantity));
            }
            goodsSku.setQuantity(quantity);

            boolean update = this.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, skuId).set(GoodsSku::getQuantity, quantity));
            if (update) {
                cache.remove(CachePrefix.GOODS.getPrefix() + goodsSku.getGoodsId());
            }
            cache.put(GoodsSkuService.getCacheKeys(skuId), goodsSku);
            cache.put(GoodsSkuService.getStockCacheKey(skuId), quantity);

            this.promotionGoodsService.updatePromotionGoodsStock(goodsSku.getId(), quantity);
        }
    }

    @Override
    public Integer getStock(String skuId) {
        String cacheKeys = GoodsSkuService.getStockCacheKey(skuId);
        Integer stock = (Integer) cache.get(cacheKeys);
        if (stock != null) {
            return stock;
        } else {
            GoodsSku goodsSku = getGoodsSkuByIdFromCache(skuId);
            cache.put(cacheKeys, goodsSku.getQuantity());
            return goodsSku.getQuantity();
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsStock(List<GoodsSku> goodsSkus) {
        Map<String, List<GoodsSku>> groupByGoodsIds = goodsSkus.stream().collect(Collectors.groupingBy(GoodsSku::getGoodsId));

        //?????????
        for (String goodsId : groupByGoodsIds.keySet()) {
            //??
            Integer quantity = 0;
            for (GoodsSku goodsSku : goodsSkus) {
                if (goodsId.equals(goodsSku.getGoodsId())) {
                    quantity += goodsSku.getQuantity();
                }
                this.updateStock(goodsSku.getId(), goodsSku.getQuantity());
            }
            //????????
            goodsService.updateStock(goodsId);
        }


    }

    /**
     * ????id????skuId???
     *
     * @param goodsId goodsId
     * @return ??skuId???
     */
    @Override
    public List<String> getSkuIdsByGoodsId(String goodsId) {
        return this.baseMapper.getGoodsSkuIdByGoodsId(goodsId);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean deleteAndInsertGoodsSkus(List<GoodsSku> goodsSkus) {
        int count = 0;
        for (GoodsSku skus : goodsSkus) {
            if (CharSequenceUtil.isEmpty(skus.getId())) {
                skus.setId(SnowFlake.getIdStr());
            }
            count = this.baseMapper.replaceGoodsSku(skus);
        }
        return count > 0;
    }

    @Override
    public Long countSkuNum(String storeId) {
        LambdaQueryWrapper<GoodsSku> queryWrapper = new LambdaQueryWrapper<>();

        queryWrapper.eq(GoodsSku::getStoreId, storeId).eq(GoodsSku::getDeleteFlag, Boolean.FALSE).eq(GoodsSku::getAuthFlag,
                GoodsAuthEnum.PASS.name()).eq(GoodsSku::getMarketEnable, GoodsStatusEnum.UPPER.name());
        return this.count(queryWrapper);
    }


    /**
     * ??????sku
     *
     * @param goodsSkuList      sku??
     * @param goodsOperationDTO ????DTO
     */
    @Override
    public void renderGoodsSkuList(List<GoodsSku> goodsSkuList, GoodsOperationDTO goodsOperationDTO) {
        // ?????????
        salesModelRenders.stream().filter(i -> i.getSalesMode().equals(goodsOperationDTO.getSalesModel())).findFirst().ifPresent(i -> i.renderBatch(goodsSkuList, goodsOperationDTO));
        for (GoodsSku goodsSku : goodsSkuList) {
            extendOldSkuValue(goodsSku);
            this.renderImages(goodsSku, goodsOperationDTO.getGoodsGalleryList());
        }
    }

    @Override
    public void updateGoodsSkuBuyCount(String skuId, int buyCount) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getId, skuId);
        updateWrapper.set(GoodsSku::getBuyCount, buyCount);
        this.update(updateWrapper);
    }

    @Override
    public void updateGoodsSkuVirtualSales(String skuId, int virtualSales) {
        GoodsSku sku = this.getById(skuId);
        if (sku == null) {
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getId, skuId);
        updateWrapper.set(GoodsSku::getVirtualSales, virtualSales);
        this.update(updateWrapper);
        sku.setVirtualSales(virtualSales);
        this.clearCache(skuId);
        goodsIndexService.buildGoodsIndex(sku);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void batchUpdateGoodsSkuVirtualSales(List<String> skuIds, int virtualSales) {
        if (skuIds == null || skuIds.isEmpty()) {
            return;
        }
        for (String skuId : skuIds) {
            this.updateGoodsSkuVirtualSales(skuId, virtualSales);
        }
    }

    @Override
    public void updateGoodsSkuGrade(String goodsId, double grade, int commentNum) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getGoodsId, goodsId);
        updateWrapper.set(GoodsSku::getGrade, grade);
        updateWrapper.set(GoodsSku::getCommentNum, commentNum);
        this.update(updateWrapper);
        this.getSkuIdsByGoodsId(goodsId).forEach(this::clearCache);
    }

    @Override
    public Integer getGoodsStock(String goodsId) {
        List<String> skuIds = this.getSkuIdsByGoodsId(goodsId);

        Integer stock = 0;

        for (String skuId : skuIds) {
            stock += this.getStock(skuId);
        }
        return stock;

    }

    @Override
    public Boolean freight(List<String> goodsId, String templateId) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.in(GoodsSku::getGoodsId, goodsId);
        updateWrapper.set(GoodsSku::getFreightTemplateId, templateId);
        updateWrapper.set(GoodsSku::getUpdateTime, new Date());
        List<String> skuIds = this.list(updateWrapper).stream().map(GoodsSku::getId).collect(Collectors.toList());
        skuIds.forEach(this::clearCache);
        return this.update(updateWrapper);
    }

    /**
     * ????sku
     *
     * @param goodsSku          sku
     * @param goodsOperationDTO ????DTO
     */
    void renderGoodsSku(GoodsSku goodsSku, GoodsOperationDTO goodsOperationDTO) {
        extendOldSkuValue(goodsSku);
        // ?????????
        salesModelRenders.stream().filter(i -> i.getSalesMode().equals(goodsOperationDTO.getSalesModel())).findFirst().ifPresent(i -> i.renderSingle(goodsSku, goodsOperationDTO));
        this.renderImages(goodsSku, goodsOperationDTO.getGoodsGalleryList());
    }

    /**
     * ??sku???????????????sku?
     *
     * @param goodsSku ??sku
     */
    private void extendOldSkuValue(GoodsSku goodsSku) {
        if (CharSequenceUtil.isNotEmpty(goodsSku.getGoodsId())) {
            GoodsSku oldSku = this.getGoodsSkuByIdFromCache(goodsSku.getId());
            if (oldSku != null) {
                goodsSku.setCommentNum(oldSku.getCommentNum());
                goodsSku.setViewCount(oldSku.getViewCount());
                goodsSku.setBuyCount(oldSku.getBuyCount());
                goodsSku.setVirtualSales(oldSku.getVirtualSales());
                goodsSku.setGrade(oldSku.getGrade());
            }
        }
    }

    /**
     * ??sku??
     *
     * @param goodsSku sku
     */
    void renderImages(GoodsSku goodsSku, List<String> goodsImages) {
        if (goodsImages == null || goodsImages.isEmpty()) {
            return;
        }
        JSONObject jsonObject = JSON.parseObject(goodsSku.getSpecs());
        List<String> images = jsonObject.getJSONArray("images").toJavaList(String.class);
        GoodsGallery goodsGallery;
        if (images != null && !images.isEmpty()) {
            goodsGallery = goodsGalleryService.getGoodsGallery(images.get(0));
        } else {
            goodsGallery = goodsGalleryService.getGoodsGallery(goodsImages.get(0));
        }

        goodsSku.setBig(goodsGallery.getOriginal());
        goodsSku.setOriginal(goodsGallery.getOriginal());
        goodsSku.setThumbnail(goodsGallery.getThumbnail());
        goodsSku.setSmall(goodsGallery.getSmall());
    }

    /**
     * ????????sku??????
     *
     * @param goodsSkuVOList ??VO??
     * @return ??????sku??????
     */
    private List<GoodsSkuSpecVO> groupBySkuAndSpec(List<GoodsSkuVO> goodsSkuVOList) {

        List<GoodsSkuSpecVO> skuSpecVOList = new ArrayList<>();
        for (GoodsSkuVO goodsSkuVO : goodsSkuVOList) {
            GoodsSkuSpecVO specVO = new GoodsSkuSpecVO();
            specVO.setSkuId(goodsSkuVO.getId());
            specVO.setSpecValues(goodsSkuVO.getSpecList());
            specVO.setQuantity(goodsSkuVO.getQuantity());
            skuSpecVOList.add(specVO);
        }
        return skuSpecVOList;
    }

    /**
     * ?????????????
     *
     * @param goodsSkuStockDTOList ???????
     * @return ????????
     */
    private XSSFWorkbook initStockExportData(List<GoodsSkuStockDTO> goodsSkuStockDTOList) {

        XSSFWorkbook workbook = new XSSFWorkbook();
        // ????
        this.createTemplate(workbook);
        // ??sku????
        this.skuStockList(workbook, goodsSkuStockDTOList);
        // ??sku????
        this.skuList(workbook, goodsSkuStockDTOList);
        return workbook;
    }

    /**
     * ????
     *
     * @param workbook
     */
    private void createTemplate(XSSFWorkbook workbook) {
        Sheet templateSheet = workbook.createSheet("????????");

        // ????
        Row description = templateSheet.createRow(0);
        description.setHeightInPoints(90);
        Cell descriptionCell = description.createCell(0);
        descriptionCell.setCellValue("??????????????\n" +
                "1.?????????????????10000?\n" +
                "2.????????????????????????????????\n" +
                "3.?????????????????0???");
        // ?????????
        templateSheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 3));

        // ???????????????????
        CellStyle descriptionStyle = workbook.createCellStyle();
        descriptionStyle.setWrapText(true);
        descriptionStyle.setAlignment(HorizontalAlignment.LEFT);
        descriptionStyle.setVerticalAlignment(VerticalAlignment.CENTER);
        descriptionCell.setCellStyle(descriptionStyle);

        // ????
        Row header = templateSheet.createRow(1);

        String[] headers = {"??ID????", "skuID????", "??????????? ? ?? ??", "??????????"};

        CellStyle headerStyle = workbook.createCellStyle();
        Font headerFont = workbook.createFont();
        headerFont.setBold(true);
        headerStyle.setFont(headerFont);
        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
            cell.setCellStyle(headerStyle);
        }
        //????
        templateSheet.setColumnWidth(0, 30 * 256);
        templateSheet.setColumnWidth(1, 30 * 256);
        templateSheet.setColumnWidth(2, 40 * 256);
        templateSheet.setColumnWidth(3, 25 * 256);

        // ??????????
        DataValidationHelper validationHelper = templateSheet.getDataValidationHelper();
        DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(new String[]{"?", "?"});
        // ??3???10002???3?
        CellRangeAddressList addressList = new CellRangeAddressList(2, 10001, 2, 2);
        DataValidation validation = validationHelper.createValidation(constraint, addressList);
        validation.setSuppressDropDownArrow(true);
        validation.setShowErrorBox(true);
        templateSheet.addValidationData(validation);
    }

    /**
     * ??sku????
     *
     * @param workbook
     */
    private void skuStockList(XSSFWorkbook workbook, List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        Sheet skuListSheet = workbook.createSheet("??????");

        // ????
        Row header = skuListSheet.createRow(0);

        String[] headers = {"??ID", "????", "??ID(SKUID)", "????", "??", "??????"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }


        // ????
        for (int i = 0; i < goodsSkuStockDTOList.size(); i++) {
            GoodsSkuStockDTO dto = goodsSkuStockDTOList.get(i);
            Row row = skuListSheet.createRow(i + 1);
            row.createCell(0).setCellValue(dto.getGoodsId());
            row.createCell(1).setCellValue(dto.getGoodsName());
            row.createCell(2).setCellValue(dto.getSkuId());
            row.createCell(3).setCellValue(dto.getSimpleSpecs());
            row.createCell(4).setCellValue(dto.getSn());
            row.createCell(5).setCellValue(dto.getQuantity());
        }

        //????
        skuListSheet.setColumnWidth(0, 30 * 256);
        skuListSheet.setColumnWidth(1, 30 * 256);
        skuListSheet.setColumnWidth(2, 30 * 256);
        skuListSheet.setColumnWidth(3, 30 * 256);
        skuListSheet.setColumnWidth(4, 30 * 256);
        skuListSheet.setColumnWidth(5, 15 * 256);
    }

    private void skuList(XSSFWorkbook workbook, List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        Sheet skuListSheet = workbook.createSheet("????");

        // ????
        Row header = skuListSheet.createRow(0);

        String[] headers = {"??ID", "????", "??ID(SKUID)", "????", "??"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }

        // ????
        for (int i = 0; i < goodsSkuStockDTOList.size(); i++) {
            GoodsSkuStockDTO dto = goodsSkuStockDTOList.get(i);
            Row row = skuListSheet.createRow(i + 1);
            row.createCell(0).setCellValue(dto.getGoodsId());
            row.createCell(1).setCellValue(dto.getGoodsName());
            row.createCell(2).setCellValue(dto.getSkuId());
            row.createCell(3).setCellValue(dto.getSimpleSpecs());
            row.createCell(4).setCellValue(dto.getSn());
        }

        //????
        skuListSheet.setColumnWidth(0, 30 * 256);
        skuListSheet.setColumnWidth(1, 30 * 256);
        skuListSheet.setColumnWidth(2, 30 * 256);
        skuListSheet.setColumnWidth(3, 30 * 256);
        skuListSheet.setColumnWidth(4, 30 * 256);
    }

    private void asyncSendIfPresent(String destination, Object payload) {
        if (liliMessageTemplate != null) {
            liliMessageTemplate.asyncSend(destination, payload, MessageSendCallbackBuilder.commonCallback());
        }
    }

}
