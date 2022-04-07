package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.date.DatePattern;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.PromotionTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.BasePromotions;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.dto.search.PromotionGoodsSearchParams;
import cn.lili.modules.promotion.entity.dto.search.SeckillSearchParams;
import cn.lili.modules.promotion.entity.enums.PromotionsApplyStatusEnum;
import cn.lili.modules.promotion.entity.vos.SeckillApplyVO;
import cn.lili.modules.promotion.entity.vos.SeckillGoodsVO;
import cn.lili.modules.promotion.entity.vos.SeckillTimelineVO;
import cn.lili.modules.promotion.mapper.SeckillApplyMapper;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.promotion.tools.PromotionTools;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 秒杀申请业务层实现
 *
 * @author Chopper
 * @since 2020/8/21
 */
@Service
@Slf4j
public class SeckillApplyServiceImpl extends ServiceImpl<SeckillApplyMapper, SeckillApply> implements SeckillApplyService {

    /**
     * 缓存
     */
    @Autowired
    private Cache<Object> cache;
    /**
     * 规格商品
     */
    @Autowired
    private GoodsSkuService goodsSkuService;
    /**
     * 促销商品
     */
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    /**
     * 秒杀
     */
    @Autowired
    private SeckillService seckillService;

    @Override
    public List<SeckillTimelineVO> getSeckillTimeline() {
        //秒杀活动缓存key
        return getSeckillTimelineInfo();
    }

    @Override
    public List<SeckillGoodsVO> getSeckillGoods(Integer timeline) {
        List<SeckillGoodsVO> seckillGoodsVoS = new ArrayList<>();
        //获取
        List<SeckillTimelineVO> seckillTimelineToCache = getSeckillTimelineInfo();
        Optional<SeckillTimelineVO> first = seckillTimelineToCache.stream().filter(i -> i.getTimeLine().equals(timeline)).findFirst();
        if (first.isPresent()) {
            seckillGoodsVoS = first.get().getSeckillGoodsList();
        }
        return seckillGoodsVoS;
    }

    @Override
    public IPage<SeckillApply> getSeckillApplyPage(SeckillSearchParams queryParam, PageVO pageVo) {
        IPage<SeckillApply> seckillApplyPage = this.page(PageUtil.initPage(pageVo), queryParam.queryWrapper());
        if (seckillApplyPage != null && !seckillApplyPage.getRecords().isEmpty()) {

            //获取skuId
            List<String> skuIds = seckillApplyPage.getRecords().stream()
                    .map(SeckillApply::getSkuId).collect(Collectors.toList());

            //循环获取 店铺/全平台 参与的促销商品库存进行填充
            if (!skuIds.isEmpty()) {
                List<Integer> skuStock = promotionGoodsService.getPromotionGoodsStock(PromotionTypeEnum.SECKILL, queryParam.getSeckillId(), skuIds);
                for (int i = 0; i < skuIds.size(); i++) {
                    seckillApplyPage.getRecords().get(i).setQuantity(skuStock.get(i));
                }
            }
        }
        return seckillApplyPage;
    }

    /**
     * 分页查询限时请购申请列表
     *
     * @param queryParam 秒杀活动申请查询参数
     * @return 限时请购申请列表
     */
    @Override
    public List<SeckillApply> getSeckillApplyList(SeckillSearchParams queryParam) {
        return this.list(queryParam.queryWrapper());
    }

    /**
     * 查询限时请购申请列表总数
     *
     * @param queryParam 查询条件
     * @return 限时请购申请列表总数
     */
    @Override
    public long getSeckillApplyCount(SeckillSearchParams queryParam) {
        return this.count(queryParam.queryWrapper());
    }

    /**
     * 查询限时请购申请
     *
     * @param queryParam 秒杀活动申请查询参数
     * @return 限时请购申请
     */
    @Override
    public SeckillApply getSeckillApply(SeckillSearchParams queryParam) {
        return this.getOne(queryParam.queryWrapper(), false);
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public void addSeckillApply(String seckillId, String storeId, List<SeckillApplyVO> seckillApplyList) {
        Seckill seckill = this.seckillService.getById(seckillId);
        if (seckill == null) {
            throw new ServiceException(ResultCode.SECKILL_NOT_EXIST_ERROR);
        }
        if (seckillApplyList == null || seckillApplyList.isEmpty()) {
            return;
        }
        //检查秒杀活动申请是否合法
        checkSeckillApplyList(seckill.getHours(), seckillApplyList);
        //获取已参与活动的秒杀活动活动申请列表
        List<String> skuIds = seckillApplyList.stream().map(SeckillApply::getSkuId).collect(Collectors.toList());
        List<SeckillApply> originList = new ArrayList<>();
        List<PromotionGoods> promotionGoodsList = new ArrayList<>();
        for (SeckillApplyVO seckillApply : seckillApplyList) {
            //获取参与活动的商品信息
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(seckillApply.getSkuId());
            if (!goodsSku.getStoreId().equals(storeId)) {
                continue;
            }
            //获取秒杀活动时间段
            DateTime startTime = DateUtil.offsetHour(seckill.getStartTime(), seckillApply.getTimeLine());
            //检测是否可以发布促销商品
            checkSeckillGoodsSku(seckill, seckillApply, goodsSku, startTime);
            //设置秒杀申请默认内容
            seckillApply.setOriginalPrice(goodsSku.getPrice());
            seckillApply.setPromotionApplyStatus(PromotionsApplyStatusEnum.PASS.name());
            seckillApply.setSalesNum(0);
            originList.add(seckillApply);
            //获取促销商品
            PromotionGoods promotionGoods = this.setSeckillGoods(goodsSku, seckillApply, seckill);
            promotionGoodsList.add(promotionGoods);
        }
        boolean result = true;
        this.remove(new LambdaQueryWrapper<SeckillApply>().eq(SeckillApply::getSeckillId, seckillId).in(SeckillApply::getSkuId, skuIds));
        this.saveBatch(originList);
        //保存促销活动商品信息
        if (!promotionGoodsList.isEmpty()) {
            PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
            searchParams.setPromotionId(seckillId);
            searchParams.setStoreId(storeId);
            searchParams.setPromotionType(PromotionTypeEnum.SECKILL.name());
            searchParams.setSkuIds(promotionGoodsList.stream().map(PromotionGoods::getSkuId).collect(Collectors.toList()));
            promotionGoodsService.deletePromotionGoods(searchParams);
            //初始化促销商品
            PromotionTools.promotionGoodsInit(promotionGoodsList, seckill, PromotionTypeEnum.SECKILL);
            result = promotionGoodsService.saveBatch(promotionGoodsList);
        }
        //设置秒杀活动的商品数量、店铺数量
        seckillService.updateSeckillGoodsNum(seckillId);
        cache.vagueDel(CachePrefix.STORE_ID_SECKILL);
        if (result) {
            this.seckillService.updateEsGoodsSeckill(seckill, originList);
        }
    }


    /**
     * 批量删除秒杀活动申请
     *
     * @param seckillId 秒杀活动活动id
     * @param id        id
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void removeSeckillApply(String seckillId, String id) {
        Seckill seckill = this.seckillService.getById(seckillId);
        if (seckill == null) {
            throw new ServiceException(ResultCode.SECKILL_NOT_EXIST_ERROR);
        }
        SeckillApply seckillApply = this.getById(id);
        if (seckillApply == null) {
            throw new ServiceException(ResultCode.SECKILL_APPLY_NOT_EXIST_ERROR);
        }


        //清除秒杀活动中的商品
        this.remove(new LambdaQueryWrapper<SeckillApply>()
                .eq(SeckillApply::getSeckillId, seckillId)
                .in(SeckillApply::getId, id));

        this.seckillService.deleteEsGoodsSeckill(seckill, Collections.singletonList(seckillApply.getSkuId()));
        //删除促销商品
        this.promotionGoodsService.deletePromotionGoods(seckillId, Collections.singletonList(seckillApply.getSkuId()));
    }

    /**
     * 更新秒杀商品出售数量
     *
     * @param seckillId 秒杀活动id
     * @param skuId     商品skuId
     * @param saleNum   库存
     */
    @Override
    public void updateSeckillApplySaleNum(String seckillId, String skuId, Integer saleNum) {
        LambdaUpdateWrapper<SeckillApply> updateWrapper = new LambdaUpdateWrapper<>();
        updateWrapper.eq(SeckillApply::getSeckillId, seckillId).eq(SeckillApply::getSkuId, skuId);
        updateWrapper.set(SeckillApply::getSalesNum, saleNum);
        this.update(updateWrapper);
    }

    /**
     * 更新秒杀活动时间
     *
     * @param seckill 秒杀活动
     * @return 是否更新成功
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public boolean updateSeckillApplyTime(Seckill seckill) {
        boolean result = false;
        List<PromotionGoods> promotionGoodsList = new ArrayList<>();
        LambdaQueryWrapper<SeckillApply> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(SeckillApply::getSeckillId, seckill.getId());
        List<SeckillApply> list = this.list(queryWrapper).stream().filter(i -> i.getTimeLine() != null && seckill.getHours().contains(i.getTimeLine().toString())).collect(Collectors.toList());
        for (SeckillApply seckillApply : list) {
            //获取参与活动的商品信息
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(seckillApply.getSkuId());
            //获取促销商品
            PromotionGoods promotionGoods = this.setSeckillGoods(goodsSku, seckillApply, seckill);
            promotionGoodsList.add(promotionGoods);
        }
        //保存促销活动商品信息
        if (!promotionGoodsList.isEmpty()) {
            PromotionGoodsSearchParams searchParams = new PromotionGoodsSearchParams();
            searchParams.setPromotionType(PromotionTypeEnum.SECKILL.name());
            searchParams.setPromotionId(seckill.getId());
            promotionGoodsService.deletePromotionGoods(searchParams);
            //初始化促销商品
            List<PromotionGoods> promotionGoods = PromotionTools.promotionGoodsInit(promotionGoodsList, seckill, PromotionTypeEnum.SECKILL);
            result = promotionGoodsService.saveBatch(promotionGoods);
            this.seckillService.updateEsGoodsSeckill(seckill, list);

            LambdaQueryWrapper<SeckillApply> deleteWrapper = new LambdaQueryWrapper<>();
            deleteWrapper.eq(SeckillApply::getSeckillId, seckill.getId());
            deleteWrapper.notIn(SeckillApply::getSkuId, promotionGoodsList.stream().map(PromotionGoods::getSkuId).collect(Collectors.toList()));
            this.remove(deleteWrapper);
        }

        seckillService.updateSeckillGoodsNum(seckill.getId());

        return result;
    }

    /**
     * 检查秒杀活动申请列表参数信息
     *
     * @param hours            秒杀活动时间段
     * @param seckillApplyList 秒杀活动申请列表
     */
    private void checkSeckillApplyList(String hours, List<SeckillApplyVO> seckillApplyList) {
        List<String> existSku = new ArrayList<>();
        for (SeckillApplyVO seckillApply : seckillApplyList) {
            if (seckillApply.getPrice() > seckillApply.getOriginalPrice()) {
                throw new ServiceException(ResultCode.SECKILL_PRICE_ERROR);
            }
            //检查秒杀活动申请的时刻，是否存在在秒杀活动的时间段内
            String[] rangeHours = hours.split(",");
            boolean containsSame = Arrays.stream(rangeHours).anyMatch(i -> i.equals(seckillApply.getTimeLine().toString()));
            if (!containsSame) {
                throw new ServiceException(ResultCode.SECKILL_TIME_ERROR);
            }
            //检查商品是否参加多个时间段的活动
            if (existSku.contains(seckillApply.getSkuId())) {
                throw new ServiceException(seckillApply.getGoodsName() + "该商品不能同时参加多个时间段的活动");
            } else {
                existSku.add(seckillApply.getSkuId());
            }

        }
    }

    /**
     * 获取秒杀活动信息
     *
     * @return 秒杀活动信息
     */
    private List<SeckillTimelineVO> getSeckillTimelineInfo() {
        List<SeckillTimelineVO> timelineList = new ArrayList<>();
        LambdaQueryWrapper<Seckill> queryWrapper = new LambdaQueryWrapper<>();
        //查询当天时间段内的秒杀活动活动
        Date now = new Date();
        queryWrapper.between(BasePromotions::getStartTime, DateUtil.beginOfDay(now), DateUtil.endOfDay(now));
        queryWrapper.ge(BasePromotions::getEndTime, DateUtil.endOfDay(now));
        List<Seckill> seckillList = this.seckillService.list(queryWrapper);
        for (Seckill seckill : seckillList) {
            //读取系统时间的时刻
            Calendar c = Calendar.getInstance();
            int hour = c.get(Calendar.HOUR_OF_DAY);
            String[] split = seckill.getHours().split(",");
            int[] hoursSored = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
            Arrays.sort(hoursSored);
            for (int i = 0; i < hoursSored.length; i++) {
                SeckillTimelineVO tempTimeline = new SeckillTimelineVO();
                boolean hoursSoredHour = (hoursSored[i] >= hour || ((i + 1) < hoursSored.length && hoursSored[i + 1] > hour) || hoursSored.length == 1);
                if (hoursSoredHour) {
                    SimpleDateFormat format = new SimpleDateFormat(DatePattern.NORM_DATE_PATTERN);
                    String date = format.format(new Date());
                    //当前时间的秒数
                    long currentTime = DateUtil.currentSeconds();
                    //秒杀活动的时刻
                    long timeLine = cn.lili.common.utils.DateUtil.getDateline(date + " " + hoursSored[i], "yyyy-MM-dd HH");

                    Long distanceTime = timeLine - currentTime < 0 ? 0 : timeLine - currentTime;
                    tempTimeline.setDistanceStartTime(distanceTime);
                    tempTimeline.setStartTime(timeLine);
                    tempTimeline.setTimeLine(hoursSored[i]);
                    tempTimeline.setSeckillGoodsList(wrapperSeckillGoods(hoursSored[i], seckill.getId()));
                    timelineList.add(tempTimeline);
                }
            }
        }
        return timelineList;
    }

    /**
     * 组装当时间秒杀活动的商品数据
     * w
     *
     * @param startTimeline 秒杀活动开始时刻
     * @return 当时间秒杀活动的商品数据
     */
    private List<SeckillGoodsVO> wrapperSeckillGoods(Integer startTimeline, String seckillId) {
        List<SeckillGoodsVO> seckillGoodsVoS = new ArrayList<>();
        List<SeckillApply> seckillApplyList = this.list(new LambdaQueryWrapper<SeckillApply>().eq(SeckillApply::getSeckillId, seckillId));
        if (!seckillApplyList.isEmpty()) {
            List<SeckillApply> collect = seckillApplyList.stream().filter(i -> i.getTimeLine().equals(startTimeline) && i.getPromotionApplyStatus().equals(PromotionsApplyStatusEnum.PASS.name())).collect(Collectors.toList());
            for (SeckillApply seckillApply : collect) {
                GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(seckillApply.getSkuId());
                if (goodsSku != null) {
                    SeckillGoodsVO goodsVO = new SeckillGoodsVO();
                    BeanUtil.copyProperties(seckillApply, goodsVO);
                    goodsVO.setGoodsImage(goodsSku.getThumbnail());
                    goodsVO.setGoodsId(goodsSku.getGoodsId());
                    goodsVO.setGoodsName(goodsSku.getGoodsName());
                    String promotionGoodsStockCacheKey = PromotionGoodsService.getPromotionGoodsStockCacheKey(
                            PromotionTypeEnum.SECKILL,
                            seckillId, seckillApply.getSkuId());
                    Object quantity = cache.get(promotionGoodsStockCacheKey);
                    if (quantity != null) {
                        goodsVO.setQuantity((Integer) quantity);
                    }
                    seckillGoodsVoS.add(goodsVO);
                }
            }
        }
        return seckillGoodsVoS;
    }

    /**
     * 检测秒杀申请的商品
     *
     * @param seckill      秒杀活动
     * @param seckillApply 秒杀活动申请
     * @param goodsSku     商品SKU
     * @param startTime    秒杀时段开启时间
     */
    private void checkSeckillGoodsSku(Seckill seckill, SeckillApplyVO seckillApply, GoodsSku goodsSku, DateTime startTime) {
        //活动库存不能大于商品库存
        if (goodsSku.getQuantity() < seckillApply.getQuantity()) {
            throw new ServiceException(seckillApply.getGoodsName() + ",此商品库存不足");
        }
        //查询是否在同一时间段参与了拼团活动
        if (promotionGoodsService.findInnerOverlapPromotionGoods(PromotionTypeEnum.PINTUAN.name(), goodsSku.getId(), startTime, seckill.getEndTime(), seckill.getId()) > 0) {
            throw new ServiceException("商品[" + goodsSku.getGoodsName() + "]已经在重叠的时间段参加了拼团活动，不能参加秒杀活动");
        }
        //查询是否在同一时间段参与了秒杀活动活动
        if (promotionGoodsService.findInnerOverlapPromotionGoods(PromotionTypeEnum.SECKILL.name(), goodsSku.getId(), startTime, seckill.getEndTime(), seckill.getId()) > 0) {
            throw new ServiceException("商品[" + goodsSku.getGoodsName() + "]已经在重叠的时间段参加了秒杀活动，不能参加秒杀活动活动");
        }
    }

    /**
     * 获取秒杀活动促销商品
     *
     * @param goodsSku     商品SKU
     * @param seckillApply 秒杀活动申请
     * @param seckill      秒杀活动
     * @return 秒杀活动促销商品
     */
    private PromotionGoods setSeckillGoods(GoodsSku goodsSku, SeckillApply seckillApply, Seckill seckill) {
        //设置促销商品默认内容
        PromotionGoods promotionGoods = new PromotionGoods(goodsSku);
        promotionGoods.setPrice(seckillApply.getPrice());
        promotionGoods.setQuantity(seckillApply.getQuantity());
        //设置单独每个促销商品的结束时间
        DateTime startTime = DateUtil.offsetHour(DateUtil.beginOfDay(seckill.getStartTime()), seckillApply.getTimeLine());
        promotionGoods.setStartTime(startTime);
        if (seckill.getEndTime() == null) {
            promotionGoods.setEndTime(DateUtil.endOfDay(startTime));
        } else {
            promotionGoods.setEndTime(seckill.getEndTime());
        }
        return promotionGoods;
    }

}