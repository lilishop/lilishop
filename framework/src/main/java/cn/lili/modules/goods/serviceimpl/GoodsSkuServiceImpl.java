package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.convert.Convert;
import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.core.util.NumberUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
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
import cn.lili.modules.search.entity.dos.EsGoodsIndex;
import cn.lili.modules.search.service.EsGoodsIndexService;
import cn.lili.mybatis.BaseEntity;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.rocketmq.RocketmqSendCallbackBuilder;
import cn.lili.rocketmq.tags.GoodsTagsEnum;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.rocketmq.spring.core.RocketMQTemplate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStream;
import java.net.URLEncoder;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 商品sku业务层实现
 *
 * @author pikachu
 * @since 2020-02-23 15:18:56
 */
@Service
public class GoodsSkuServiceImpl extends ServiceImpl<GoodsSkuMapper, GoodsSku> implements GoodsSkuService {

    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 分类
     */
    @Autowired
    private MemberCouponService memberCouponService;
    /**
     * 商品相册
     */
    @Autowired
    private GoodsGalleryService goodsGalleryService;
    /**
     * rocketMq
     */
    @Autowired
    private RocketMQTemplate rocketMQTemplate;
    /**
     * rocketMq配置
     */
    @Autowired
    private RocketmqCustomProperties rocketmqCustomProperties;
    /**
     * 商品
     */
    @Autowired
    private GoodsService goodsService;
    /**
     * 商品索引
     */
    @Autowired
    private EsGoodsIndexService goodsIndexService;

    @Autowired
    private PromotionGoodsService promotionGoodsService;

    @Autowired
    private ApplicationEventPublisher applicationEventPublisher;

    @Autowired
    private WholesaleService wholesaleService;

    @Autowired
    private CouponService couponService;

    @Autowired
    private List<SalesModelRender> salesModelRenders;

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void add(Goods goods, GoodsOperationDTO goodsOperationDTO) {
        // 是否存在规格
        if (goodsOperationDTO.getSkuList() == null || goodsOperationDTO.getSkuList().isEmpty()) {
            throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU);
        }
        // 检查是否需要生成索引
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
        // 是否存在规格
        if (goodsOperationDTO.getSkuList() == null || goodsOperationDTO.getSkuList().isEmpty()) {
            throw new ServiceException(ResultCode.MUST_HAVE_GOODS_SKU);
        }
        List<GoodsSku> skuList;
        //删除旧的sku信息
        if (Boolean.TRUE.equals(goodsOperationDTO.getRegeneratorSkuFlag())) {
            skuList = GoodsSkuBuilder.buildBatch(goods, goodsOperationDTO.getSkuList());
            renderGoodsSkuList(skuList, goodsOperationDTO);
            List<GoodsSkuVO> goodsListByGoodsId = getGoodsListByGoodsId(goods.getId());
            List<String> oldSkuIds = new ArrayList<>();
            //删除旧索引
            for (GoodsSkuVO goodsSkuVO : goodsListByGoodsId) {
                oldSkuIds.add(goodsSkuVO.getId());
                cache.remove(GoodsSkuService.getCacheKeys(goodsSkuVO.getId()));
            }

            //删除sku相册
            goodsGalleryService.removeByGoodsId(goods.getId());

            //发送mq消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.SKU_DELETE.name();
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(oldSkuIds),
                    RocketmqSendCallbackBuilder.commonCallback());
        } else {
            skuList = new ArrayList<>();
            for (Map<String, Object> map : goodsOperationDTO.getSkuList()) {
                GoodsSku sku = GoodsSkuBuilder.build(goods, map);
                renderGoodsSku(sku, goodsOperationDTO);
                skuList.add(sku);
                //如果商品状态值不对，则es索引移除
                if (goods.getAuthFlag().equals(GoodsAuthEnum.PASS.name()) && goods.getMarketEnable().equals(GoodsStatusEnum.UPPER.name())) {
                    goodsIndexService.deleteIndexById(sku.getId());
                }
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
     * 更新商品sku
     *
     * @param goodsSku sku信息
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void update(GoodsSku goodsSku) {
        this.updateById(goodsSku);
        cache.remove(GoodsSkuService.getCacheKeys(goodsSku.getId()));
        cache.put(GoodsSkuService.getCacheKeys(goodsSku.getId()), goodsSku);
    }


    /**
     * 清除sku缓存
     *
     * @param skuId skuID
     */
    @Override
    public void clearCache(String skuId) {
        cache.remove(GoodsSkuService.getCacheKeys(skuId));
    }

    @Override
    public GoodsSku getGoodsSkuByIdFromCache(String id) {
        //获取缓存中的sku
        GoodsSku goodsSku = (GoodsSku) cache.get(GoodsSkuService.getCacheKeys(id));
        //如果缓存中没有信息，则查询数据库，然后写入缓存
        if (goodsSku == null) {
            goodsSku = this.getById(id);
            if (goodsSku == null) {
                return null;
            }
            cache.put(GoodsSkuService.getCacheKeys(id), goodsSku);
        }

        //获取商品库存
        Integer integer = (Integer) cache.get(GoodsSkuService.getStockCacheKey(id));

        //库存不为空,库存与缓存中不一致
        if (integer != null && !goodsSku.getQuantity().equals(integer)) {
            //写入最新的库存信息
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
        //获取商品VO
        GoodsVO goodsVO = goodsService.getGoodsVO(goodsId);
        //如果skuid为空，则使用商品VO中sku信息获取
        if (CharSequenceUtil.isEmpty(skuId) || "undefined".equals(skuId)) {
            skuId = goodsVO.getSkuList().get(0).getId();
        }
        //从缓存拿商品Sku
        GoodsSku goodsSku = this.getGoodsSkuByIdFromCache(skuId);
        //如果使用商品ID无法查询SKU则返回错误
        if (goodsVO == null || goodsSku == null) {
            //发送mq消息
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_DELETE.name();
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(Collections.singletonList(goodsId)),
                    RocketmqSendCallbackBuilder.commonCallback());
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        //商品下架||商品未审核通过||商品删除，则提示：商品已下架
        if (GoodsStatusEnum.DOWN.name().equals(goodsVO.getMarketEnable()) || !GoodsAuthEnum.PASS.name().equals(goodsVO.getAuthFlag()) || Boolean.TRUE.equals(goodsVO.getDeleteFlag())) {
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.GOODS_DELETE.name();
            rocketMQTemplate.asyncSend(destination, JSONUtil.toJsonStr(Collections.singletonList(goodsId)),
                    RocketmqSendCallbackBuilder.commonCallback());
            throw new ServiceException(ResultCode.GOODS_NOT_EXIST);
        }

        //获取当前商品的索引信息
        EsGoodsIndex goodsIndex = goodsIndexService.findById(skuId);
        if (goodsIndex == null) {
            goodsIndex = goodsIndexService.getResetEsGoodsIndex(goodsSku, goodsVO.getGoodsParamsDTOList());
        }

        //商品规格
        GoodsSkuVO goodsSkuDetail = this.getGoodsSkuVO(goodsSku);

        Map<String, Object> promotionMap = goodsIndex.getPromotionMap();
        AuthUser currentUser = UserContext.getCurrentUser();
        //设置当前商品的促销价格
        if (promotionMap != null && !promotionMap.isEmpty()) {
            promotionMap = promotionMap.entrySet().stream().parallel().filter(i -> {
                JSONObject jsonObject = JSONUtil.parseObj(i.getValue());
                if (i.getKey().contains(PromotionTypeEnum.COUPON.name()) && currentUser != null) {
                    Integer couponLimitNum = jsonObject.getInt("couponLimitNum");
                    Coupon coupon = couponService.getById(jsonObject.getStr("id"));
                    if (coupon == null || (coupon.getPublishNum() != 0 && coupon.getReceivedNum() >= coupon.getPublishNum())) {
                        return false;
                    }
                    if (couponLimitNum > 0) {
                        Long count = memberCouponService.getMemberCouponNum(currentUser.getId(), jsonObject.getStr(
                                "id"));
                        if (count >= couponLimitNum) {
                            return false;
                        }
                    }
                }
                // 过滤活动赠送优惠券和无效时间的活动
                return (jsonObject.get("getType") == null || jsonObject.get("getType", String.class).equals(CouponGetEnum.FREE.name())) && (jsonObject.get("startTime") != null && jsonObject.get("startTime", Date.class).getTime() <= System.currentTimeMillis()) && (jsonObject.get("endTime") == null || jsonObject.get("endTime", Date.class).getTime() >= System.currentTimeMillis());
            }).collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));

            Optional<Map.Entry<String, Object>> containsPromotion =
                    promotionMap.entrySet().stream().filter(i -> i.getKey().contains(PromotionTypeEnum.SECKILL.name()) || i.getKey().contains(PromotionTypeEnum.PINTUAN.name())).findFirst();
            if (containsPromotion.isPresent()) {
                JSONObject jsonObject = JSONUtil.parseObj(containsPromotion.get().getValue());
                PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
                searchParams.setSkuId(skuId);
                searchParams.setPromotionId(jsonObject.get("id").toString());
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

        //获取分类
        map.put("wholesaleList", GoodsSalesModeEnum.WHOLESALE.name().equals(goodsVO.getSalesModel()) ?
                wholesaleService.findByGoodsId(goodsSkuDetail.getGoodsId()) : Collections.emptyList());
        map.put("categoryName", CharSequenceUtil.isNotEmpty(goodsIndex.getCategoryNamePath()) ?
                goodsIndex.getCategoryNamePath().split(",") : null);

        //获取规格信息
        map.put("specs", this.groupBySkuAndSpec(goodsVO.getSkuList()));
        map.put("promotionMap", promotionMap);

        //获取参数信息
        if (goodsVO.getGoodsParamsDTOList() != null && !goodsVO.getGoodsParamsDTOList().isEmpty()) {
            map.put("goodsParamsDTOList", goodsVO.getGoodsParamsDTOList());
        }

        //记录用户足迹
        if (currentUser != null) {
            FootPrint footPrint = new FootPrint(currentUser.getId(), goodsIndex.getStoreId(), goodsId, skuId);
            String destination = rocketmqCustomProperties.getGoodsTopic() + ":" + GoodsTagsEnum.VIEW_GOODS.name();
            rocketMQTemplate.asyncSend(destination, footPrint, RocketmqSendCallbackBuilder.commonCallback());
        }
        return map;
    }

    /**
     * 更新商品sku状态
     *
     * @param goods 商品信息(Id,MarketEnable/AuthFlag)
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
     * 更新商品sku状态根据店铺id
     *
     * @param storeId      店铺id
     * @param marketEnable 市场启用状态
     * @param authFlag     审核状态
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateGoodsSkuStatusByStoreId(String storeId, String marketEnable, String authFlag) {
        LambdaUpdateWrapper<GoodsSku> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(GoodsSku::getStoreId, storeId);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(marketEnable), GoodsSku::getMarketEnable, marketEnable);
        updateWrapper.set(CharSequenceUtil.isNotEmpty(authFlag), GoodsSku::getAuthFlag, authFlag);
        boolean update = this.update(updateWrapper);
        if (Boolean.TRUE.equals(update)) {
            if (GoodsStatusEnum.UPPER.name().equals(marketEnable)) {
                applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("生成店铺商品",
                        rocketmqCustomProperties.getGoodsTopic(), GoodsTagsEnum.GENERATOR_STORE_GOODS_INDEX.name(),
                        storeId));
            } else if (GoodsStatusEnum.DOWN.name().equals(marketEnable)) {
                cache.vagueDel(CachePrefix.GOODS_SKU.getPrefix());
                applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("删除店铺商品",
                        rocketmqCustomProperties.getGoodsTopic(), GoodsTagsEnum.STORE_GOODS_DELETE.name(), storeId));
            }
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
     * 获取goodsId下所有的goodsSku
     *
     * @param goodsId 商品id
     * @return goodsSku列表
     */
    @Override
    public List<GoodsSku> getGoodsSkuListByGoodsId(String goodsId) {
        return this.list(new LambdaQueryWrapper<GoodsSku>().eq(GoodsSku::getGoodsId, goodsId));
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
        //初始化商品
        GoodsSkuVO goodsSkuVO = new GoodsSkuVO(goodsSku);
        //获取sku信息
        JSONObject jsonObject = JSONUtil.parseObj(goodsSku.getSpecs());
        //用于接受sku信息
        List<SpecValueVO> specValueVOS = new ArrayList<>();
        //用于接受sku相册
        List<String> goodsGalleryList = new ArrayList<>();
        //循环提交的sku表单
        for (Map.Entry<String, Object> entry : jsonObject.entrySet()) {
            SpecValueVO specValueVO = new SpecValueVO();
            if ("images".equals(entry.getKey())) {
                specValueVO.setSpecName(entry.getKey());
                List<String> specImages = JSONUtil.toList(JSONUtil.parseArray(entry.getValue()),
                        String.class);
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
            // 设置响应头
            String fileName = URLEncoder.encode("商品库存", "UTF-8");
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
            // 使用 WorkbookFactory.create 方法读取 Excel 文件
            Workbook workbook = WorkbookFactory.create(inputStream);
            // 我们只读取第一个sheet
            Sheet sheet = workbook.getSheetAt(0);

            // 检查第一个sheet的行数是否超过10002行
            if (sheet.getPhysicalNumberOfRows() > 10002) {
                throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "Excel行数超过10002行");
            }
            // 遍历行和单元格
            Iterator<Row> rowIterator = sheet.rowIterator();
            int rowIndex = 0;
            while (rowIterator.hasNext()) {
                Row row = rowIterator.next();
                rowIndex++;

                // 跳过表头
                if (rowIndex < 3) {
                    continue;
                }

                List<Object> objects = new ArrayList<>();
                for (int i = 0; i < 4; i++) {
                    objects.add(getCellValue(row.getCell(i)));
                }
                log.error(getCellValue(row.getCell(2)));
                log.error(getCellValue(row.getCell(3)));
                // 判断数据格式
                if (!"增".equals(getCellValue(row.getCell(2))) && !"减".equals(getCellValue(row.getCell(2)))) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "库存修改方向列数据必须是“增”或“减”");
                } else if (!NumberUtil.isInteger(getCellValue(row.getCell(3))) || Integer.parseInt(getCellValue(row.getCell(3))) < 0) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "库存必须是正整数");
                } else if (this.count(new LambdaQueryWrapper<GoodsSku>()
                        .eq(GoodsSku::getGoodsId, getCellValue(row.getCell(0)))
                        .eq(GoodsSku::getId, getCellValue(row.getCell(1)))
                        .eq(GoodsSku::getStoreId, storeId)) == 0) {
                    throw new ServiceException(ResultCode.GOODS_STOCK_IMPORT_ERROR, "第" + rowIndex + "行商品不存在");
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

        // 批量修改商品库存
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
                    // 将数值转换为整数以去掉小数点
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
        return this.baseMapper.queryByParams(page, queryWrapper);
    }

    /**
     * 列表查询商品sku信息
     *
     * @param searchParams 查询参数
     * @return 商品sku信息
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

        //统计每个商品的库存
        for (Map.Entry<String, List<GoodsSkuStockDTO>> entry : groupByGoodsIds.entrySet()) {
            //库存
            for (GoodsSkuStockDTO goodsSku : entry.getValue()) {
                goodsSkuStockDTOS.stream().filter(i -> i.getSkuId().equals(goodsSku.getSkuId())).findFirst().ifPresent(i -> goodsSku.setQuantity(i.getQuantity()));

                this.updateStock(goodsSku.getSkuId(), goodsSku.getQuantity());
            }
            //保存商品库存结果
            goodsService.updateStock(entry.getKey());
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStocksByType(List<GoodsSkuStockDTO> goodsSkuStockDTOS) {

        // 获取所有的goodsID，并去除相同的goodsID
        List<String> goodsIds = goodsSkuStockDTOS.stream()
                .map(GoodsSkuStockDTO::getGoodsId)
                .distinct()
                .collect(Collectors.toList());

        //更新SKU库存
        for (GoodsSkuStockDTO goodsSkuStockDTO : goodsSkuStockDTOS) {
            this.updateStock(goodsSkuStockDTO.getSkuId(), goodsSkuStockDTO.getQuantity(), goodsSkuStockDTO.getType());
        }
        //更新SPU库存
        for (String goodsId : goodsIds) {
            goodsService.updateStock(goodsId);
        }
    }


    @Override
    public void updateAlertQuantity(GoodsSkuStockDTO goodsSkuStockDTO) {
        GoodsSku goodsSku = this.getById(goodsSkuStockDTO.getSkuId());
        goodsSku.setAlertQuantity(goodsSkuStockDTO.getAlertQuantity());
        //清除缓存，防止修改预警后直接修改商品导致预警数值错误
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
        // 将去重后的元素转回列表
        List<String> uniqueGoodsIdList = new ArrayList<>(uniqueSet);
        for (String goodsId : uniqueGoodsIdList) {
            cache.remove(CachePrefix.GOODS.getPrefix() + goodsId);
        }
        //修改预警库存
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
            //判断商品sku是否已经下架(修改商品库存为0时  会自动下架商品),再次更新商品库存时 需更新商品索引
            boolean isFlag = goodsSku.getQuantity() <= 0;

            goodsSku.setQuantity(quantity);
            boolean update = this.update(new LambdaUpdateWrapper<GoodsSku>().eq(GoodsSku::getId, skuId).set(GoodsSku::getQuantity, quantity));
            if (update) {
                cache.remove(CachePrefix.GOODS.getPrefix() + goodsSku.getGoodsId());
            }
            cache.put(GoodsSkuService.getCacheKeys(skuId), goodsSku);
            cache.put(GoodsSkuService.getStockCacheKey(skuId), quantity);

            this.promotionGoodsService.updatePromotionGoodsStock(goodsSku.getId(), quantity);
            //商品库存为0是删除商品索引
            if (quantity <= 0) {
                goodsIndexService.deleteIndexById(goodsSku.getId());
            }
            //库存从<=0恢复到>0并且商品状态为上架时重建商品索引
            if (isFlag && quantity > 0 && CharSequenceUtil.equals(goodsSku.getMarketEnable(), GoodsStatusEnum.UPPER.name())) {
                List<String> goodsIds = new ArrayList<>();
                goodsIds.add(goodsSku.getGoodsId());
                applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("更新商品", rocketmqCustomProperties.getGoodsTopic(),
                        GoodsTagsEnum.UPDATE_GOODS_INDEX.name(), goodsIds));
            }
        }
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void updateStock(String skuId, Integer quantity, String type) {
        GoodsSku goodsSku = getGoodsSkuByIdFromCache(skuId);
        if (goodsSku != null) {
            //判断商品sku是否已经下架(修改商品库存为0时  会自动下架商品),再次更新商品库存时 需更新商品索引
            boolean isFlag = goodsSku.getQuantity() <= 0;

            //计算修改库存
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
            //商品库存为0是删除商品索引
            if (quantity <= 0) {
                goodsIndexService.deleteIndexById(goodsSku.getId());
            }
            //库存从<=0恢复到>0并且商品状态为上架时重建商品索引
            if (isFlag && quantity > 0 && CharSequenceUtil.equals(goodsSku.getMarketEnable(), GoodsStatusEnum.UPPER.name())) {
                List<String> goodsIds = new ArrayList<>();
                goodsIds.add(goodsSku.getGoodsId());
                applicationEventPublisher.publishEvent(new TransactionCommitSendMQEvent("更新商品", rocketmqCustomProperties.getGoodsTopic(),
                        GoodsTagsEnum.UPDATE_GOODS_INDEX.name(), goodsIds));
            }
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

        //统计每个商品的库存
        for (String goodsId : groupByGoodsIds.keySet()) {
            //库存
            Integer quantity = 0;
            for (GoodsSku goodsSku : goodsSkus) {
                if (goodsId.equals(goodsSku.getGoodsId())) {
                    quantity += goodsSku.getQuantity();
                }
                this.updateStock(goodsSku.getId(), goodsSku.getQuantity());
            }
            //保存商品库存结果
            goodsService.updateStock(goodsId);
        }


    }

    /**
     * 根据商品id获取全部skuId的集合
     *
     * @param goodsId goodsId
     * @return 全部skuId的集合
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
     * 批量渲染商品sku
     *
     * @param goodsSkuList      sku集合
     * @param goodsOperationDTO 商品操作DTO
     */
    @Override
    public void renderGoodsSkuList(List<GoodsSku> goodsSkuList, GoodsOperationDTO goodsOperationDTO) {
        // 商品销售模式渲染器
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
     * 渲染商品sku
     *
     * @param goodsSku          sku
     * @param goodsOperationDTO 商品操作DTO
     */
    void renderGoodsSku(GoodsSku goodsSku, GoodsOperationDTO goodsOperationDTO) {
        extendOldSkuValue(goodsSku);
        // 商品销售模式渲染器
        salesModelRenders.stream().filter(i -> i.getSalesMode().equals(goodsOperationDTO.getSalesModel())).findFirst().ifPresent(i -> i.renderSingle(goodsSku, goodsOperationDTO));
        this.renderImages(goodsSku, goodsOperationDTO.getGoodsGalleryList());
    }

    /**
     * 将原sku的一些不会直接传递的值放到新的sku中
     *
     * @param goodsSku 商品sku
     */
    private void extendOldSkuValue(GoodsSku goodsSku) {
        if (CharSequenceUtil.isNotEmpty(goodsSku.getGoodsId())) {
            GoodsSku oldSku = this.getGoodsSkuByIdFromCache(goodsSku.getId());
            if (oldSku != null) {
                goodsSku.setCommentNum(oldSku.getCommentNum());
                goodsSku.setViewCount(oldSku.getViewCount());
                goodsSku.setBuyCount(oldSku.getBuyCount());
                goodsSku.setGrade(oldSku.getGrade());
            }
        }
    }

    /**
     * 渲染sku图片
     *
     * @param goodsSku sku
     */
    void renderImages(GoodsSku goodsSku, List<String> goodsImages) {
        if (goodsImages == null || goodsImages.isEmpty()) {
            return;
        }
        JSONObject jsonObject = JSONUtil.parseObj(goodsSku.getSpecs());
        List<String> images = jsonObject.getBeanList("images", String.class);
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
     * 根据商品分组商品sku及其规格信息
     *
     * @param goodsSkuVOList 商品VO列表
     * @return 分组后的商品sku及其规格信息
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
     * 初始化填充商品库存导出数据
     *
     * @param goodsSkuStockDTOList 导出的库存数据
     * @return 商品库存导出列表
     */
    private XSSFWorkbook initStockExportData(List<GoodsSkuStockDTO> goodsSkuStockDTOList) {

        XSSFWorkbook workbook = new XSSFWorkbook();
        // 创建模板
        this.createTemplate(workbook);
        // 创建sku库存列表
        this.skuStockList(workbook, goodsSkuStockDTOList);
        // 创建sku库存列表
        this.skuList(workbook, goodsSkuStockDTOList);
        return workbook;
    }

    /**
     * 创建模板
     *
     * @param workbook
     */
    private void createTemplate(XSSFWorkbook workbook) {
        Sheet templateSheet = workbook.createSheet("商品库存编辑模板");

        // 创建表头
        Row description = templateSheet.createRow(0);
        description.setHeightInPoints(90);
        Cell descriptionCell = description.createCell(0);
        descriptionCell.setCellValue("填写说明（请勿删除本说明）：\n" +
                "1.可批量设置多个商品的库存，一次最多10000行\n" +
                "2.库存修改方向：选择库存方向后，会在原先库存基础上进行增加或者减少\n" +
                "3.库存变更数量：需要为整数，不能填写0和负数");
        // 合并描述行的单元格
        templateSheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 3));

        // 设置描述行单元格样式（例如，自动换行）
        CellStyle descriptionStyle = workbook.createCellStyle();
        descriptionStyle.setWrapText(true);
        descriptionStyle.setAlignment(HorizontalAlignment.LEFT);
        descriptionStyle.setVerticalAlignment(VerticalAlignment.CENTER);
        descriptionCell.setCellStyle(descriptionStyle);

        // 创建表头
        Row header = templateSheet.createRow(1);

        String[] headers = {"商品ID（必填）", "skuID（必填）", "库存修改方向（必填，填 增 或者 减）", "库存变更数量（必填）"};

        CellStyle headerStyle = workbook.createCellStyle();
        Font headerFont = workbook.createFont();
        headerFont.setBold(true);
        headerStyle.setFont(headerFont);
        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
            cell.setCellStyle(headerStyle);
        }
        //修改列宽
        templateSheet.setColumnWidth(0, 30 * 256);
        templateSheet.setColumnWidth(1, 30 * 256);
        templateSheet.setColumnWidth(2, 40 * 256);
        templateSheet.setColumnWidth(3, 25 * 256);

        // 设置下拉列表数据验证
        DataValidationHelper validationHelper = templateSheet.getDataValidationHelper();
        DataValidationConstraint constraint = validationHelper.createExplicitListConstraint(new String[]{"增", "减"});
        // 从第3行到第10002行，第3列
        CellRangeAddressList addressList = new CellRangeAddressList(2, 10001, 2, 2);
        DataValidation validation = validationHelper.createValidation(constraint, addressList);
        validation.setSuppressDropDownArrow(true);
        validation.setShowErrorBox(true);
        templateSheet.addValidationData(validation);
    }

    /**
     * 创建sku库存列表
     *
     * @param workbook
     */
    private void skuStockList(XSSFWorkbook workbook, List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        Sheet skuListSheet = workbook.createSheet("商品库存信息");

        // 创建表头
        Row header = skuListSheet.createRow(0);

        String[] headers = {"商品ID", "商品名称", "规格ID(SKUID)", "规格名称", "货号", "当前库存数量"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }


        // 填充数据
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

        //修改列宽
        skuListSheet.setColumnWidth(0, 30 * 256);
        skuListSheet.setColumnWidth(1, 30 * 256);
        skuListSheet.setColumnWidth(2, 30 * 256);
        skuListSheet.setColumnWidth(3, 30 * 256);
        skuListSheet.setColumnWidth(4, 30 * 256);
        skuListSheet.setColumnWidth(5, 15 * 256);
    }

    private void skuList(XSSFWorkbook workbook, List<GoodsSkuStockDTO> goodsSkuStockDTOList) {
        Sheet skuListSheet = workbook.createSheet("商品规格");

        // 创建表头
        Row header = skuListSheet.createRow(0);

        String[] headers = {"商品ID", "商品名称", "规格ID(SKUID)", "规格名称", "货号"};

        for (int i = 0; i < headers.length; i++) {
            Cell cell = header.createCell(i);
            cell.setCellValue(headers[i]);
        }

        // 填充数据
        for (int i = 0; i < goodsSkuStockDTOList.size(); i++) {
            GoodsSkuStockDTO dto = goodsSkuStockDTOList.get(i);
            Row row = skuListSheet.createRow(i + 1);
            row.createCell(0).setCellValue(dto.getGoodsId());
            row.createCell(1).setCellValue(dto.getGoodsName());
            row.createCell(2).setCellValue(dto.getSkuId());
            row.createCell(3).setCellValue(dto.getSimpleSpecs());
            row.createCell(4).setCellValue(dto.getSn());
        }

        //修改列宽
        skuListSheet.setColumnWidth(0, 30 * 256);
        skuListSheet.setColumnWidth(1, 30 * 256);
        skuListSheet.setColumnWidth(2, 30 * 256);
        skuListSheet.setColumnWidth(3, 30 * 256);
        skuListSheet.setColumnWidth(4, 30 * 256);
    }

}
