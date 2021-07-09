package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.convert.Convert;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.date.DateUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.promotion.entity.dos.PromotionGoods;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.dos.SeckillApply;
import cn.lili.modules.promotion.entity.enums.PromotionApplyStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionTypeEnum;
import cn.lili.modules.promotion.entity.vos.*;
import cn.lili.modules.promotion.mapper.SeckillApplyMapper;
import cn.lili.modules.promotion.service.PromotionGoodsService;
import cn.lili.modules.promotion.service.SeckillApplyService;
import cn.lili.modules.promotion.service.SeckillService;
import cn.lili.modules.promotion.tools.PromotionCacheKeys;
import cn.lili.modules.promotion.tools.PromotionTools;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.text.SimpleDateFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 秒杀申请业务层实现
 *
 * @author Chopper
 * @date 2020/8/21
 */
@Service
@Transactional(rollbackFor = Exception.class)
public class SeckillApplyServiceImpl extends ServiceImpl<SeckillApplyMapper, SeckillApply> implements SeckillApplyService {

    /**
     * 缓存
     */
    @Autowired
    private Cache<Object> cache;
    /**
     * Mongo
     */
    @Autowired
    private MongoTemplate mongoTemplate;
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
        List<SeckillTimelineVO> timelineVoS = new ArrayList<>();
        //秒杀活动缓存key
        String seckillCacheKey = PromotionCacheKeys.getSeckillTimelineKey(DateUtil.format(DateUtil.beginOfDay(new DateTime()), "yyyyMMdd"));
        Map<Object, Object> cacheSeckill = cache.getHash(seckillCacheKey);
        if (cacheSeckill == null || cacheSeckill.isEmpty()) {
            //如缓存中不存在，则单独获取
            try {
                timelineVoS = getSeckillTimelineToCache(seckillCacheKey);
            } catch (Exception e) {
                log.error("获取秒杀活动信息发生错误！", e);
            }
        } else {
            //如缓存中存在，则取缓存中转为展示的信息
            for (Object value : cacheSeckill.values()) {
                SeckillTimelineVO seckillTimelineVO = (SeckillTimelineVO) value;
                timelineVoS.add(seckillTimelineVO);
            }
        }
        return timelineVoS;
    }

    @Override
    public List<SeckillGoodsVO> getSeckillGoods(Integer timeline) {
        List<SeckillGoodsVO> seckillGoodsVoS = new ArrayList<>();
        //秒杀活动缓存key
        String seckillCacheKey = PromotionCacheKeys.getSeckillTimelineKey(DateUtil.format(DateUtil.beginOfDay(new DateTime()), "yyyyMMdd"));
        Map<Object, Object> cacheSeckill = cache.getHash(seckillCacheKey);
        if (cacheSeckill == null || cacheSeckill.isEmpty()) {
            //如缓存中不存在，则单独获取
            seckillGoodsVoS = wrapperSeckillGoods(timeline);
        } else {
            //如缓存中存在，则取缓存中转为展示的信息
            for (Map.Entry<Object, Object> entry : cacheSeckill.entrySet()) {
                Integer timelineKey = Convert.toInt(entry.getKey().toString());
                if (timelineKey.equals(timeline)) {
                    seckillGoodsVoS = (List<SeckillGoodsVO>) entry.getValue();
                }
            }
        }
        return seckillGoodsVoS;
    }

    @Override
    public IPage<SeckillApply> getSeckillApplyFromMongo(SeckillSearchParams queryParam, PageVO pageVo) {
        IPage<SeckillApply> seckillApplyPage = new Page<>();
        Query query = queryParam.mongoQuery();

        SeckillVO seckillVO = this.mongoTemplate.findOne(query, SeckillVO.class);
        if (seckillVO != null && pageVo != null) {
            seckillApplyPage.setCurrent(pageVo.getMongoPageNumber());
            seckillApplyPage.setSize(pageVo.getPageSize());
            List<SeckillApply> seckillApplyList = seckillVO.getSeckillApplyList() != null ? seckillVO.getSeckillApplyList() : new ArrayList<>();
            for (SeckillApply seckillApply : seckillApplyList) {
                if (CharSequenceUtil.isNotEmpty(queryParam.getStoreId()) && !seckillApply.getStoreId().equals(queryParam.getStoreId())) {
                    seckillApplyList.remove(seckillApply);
                }
                try {
                    Integer goodsStock = promotionGoodsService.getPromotionGoodsStock(PromotionTypeEnum.SECKILL, seckillApply.getSeckillId(), seckillApply.getSkuId());
                    seckillApply.setQuantity(goodsStock);
                } catch (Exception e) {
                    log.error("获取促销商品促销失败！", e);
                }
            }
            seckillApplyPage.setTotal(seckillApplyList.size());
            List<SeckillApply> page = CollUtil.page(pageVo.getMongoPageNumber(), pageVo.getPageSize(), seckillApplyList);
            seckillApplyPage.setRecords(page);
            return seckillApplyPage;
        } else {
            return null;
        }
    }

    @Override
    public void addSeckillApply(String seckillId, String storeId, List<SeckillApplyVO> seckillApplyList) {
        SeckillVO seckill = mongoTemplate.findById(seckillId, SeckillVO.class);
        if (seckill == null) {
            throw new ServiceException(ResultCode.SECKILL_NOT_EXIST_ERROR);
        }
        //检查秒杀活动申请是否合法
        checkSeckillApplyList(seckill.getHours(), seckillApplyList, storeId);
        //获取已参与活动的秒杀活动活动申请列表
        List<SeckillApply> originList = seckill.getSeckillApplyList() != null ? seckill.getSeckillApplyList() : new ArrayList<>();
        List<PromotionGoods> promotionGoodsList = new ArrayList<>();
        for (SeckillApplyVO seckillApply : seckillApplyList) {
            //获取参与活动的商品信息
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(seckillApply.getSkuId());
            //获取秒杀活动时间段
            DateTime startTime = DateUtil.offsetHour(seckill.getStartTime(), seckillApply.getTimeLine());
            //检测是否可以发布促销商品
            checkSeckillGoodsSku(seckill, seckillApply, goodsSku, startTime);
            //设置秒杀申请默认内容
            seckillApply.setOriginalPrice(goodsSku.getPrice());
            seckillApply.setPromotionApplyStatus(PromotionApplyStatusEnum.PASS.name());
            seckillApply.setSalesNum(0);
            originList.add(seckillApply);
            //获取促销商品
            PromotionGoods promotionGoods = this.setSeckillGoods(goodsSku, seckillApply, seckill);
            promotionGoodsList.add(promotionGoods);
        }
        //保存秒杀活动申请
        this.saveOrUpdateBatch(originList);
        //设置秒杀活动下的申请列表
        seckill.setSeckillApplyList(originList);
        //mongo保存秒杀活动信息
        this.mongoTemplate.save(seckill);
        //保存促销活动商品信息
        if (!promotionGoodsList.isEmpty()) {
            LambdaQueryWrapper<PromotionGoods> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.in(PromotionGoods::getSkuId, promotionGoodsList.stream().map(PromotionGoods::getSkuId).collect(Collectors.toList()))
                    .eq(PromotionGoods::getStoreId, storeId);
            promotionGoodsService.remove(queryWrapper);
            //初始化促销商品
            PromotionTools.promotionGoodsInit(promotionGoodsList, seckill, PromotionTypeEnum.SECKILL);
            promotionGoodsService.saveBatch(promotionGoodsList);
        }
        //设置秒杀活动的商品数量、店铺数量
        seckillService.updateSeckillGoodsNum(seckillId);
    }


    /**
     * 批量删除秒杀活动申请
     *
     * @param seckillId 秒杀活动活动id
     * @param ids       秒杀活动申请id集合
     */
    @Override
    public void removeSeckillApplyByIds(String seckillId, List<String> ids) {
        SeckillVO seckillVO = this.mongoTemplate.findById(seckillId, SeckillVO.class);
        if (seckillVO == null) {
            throw new ServiceException(ResultCode.SECKILL_NOT_EXIST_ERROR);
        }
        if (seckillVO.getPromotionStatus().equals(PromotionStatusEnum.START.name())) {
            throw new ServiceException(ResultCode.SECKILL_UPDATE_ERROR);
        }
        seckillVO.getSeckillApplyList().removeIf(seckillApply -> ids.contains(seckillApply.getId()));
        this.mongoTemplate.save(seckillVO);
        this.removeByIds(ids);
    }

    /**
     * 检查秒杀活动申请列表参数信息
     *
     * @param hours            秒杀活动时间段
     * @param seckillApplyList 秒杀活动申请列表
     * @param storeId          当前申请商家编号
     */
    private void checkSeckillApplyList(String hours, List<SeckillApplyVO> seckillApplyList, String storeId) {
        List<String> existSku = new ArrayList<>();
        for (SeckillApplyVO seckillApply : seckillApplyList) {
            seckillApply.setStoreId(storeId);
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
     * 组装促销商品信息
     *
     * @param seckillApply     秒杀活动申请信息
     * @param seckillStartTime 当前秒杀活动申请的开始时间
     * @return 促销商品信息
     */
    private PromotionGoods setPromotionGoods(SeckillApply seckillApply, Date seckillStartTime) {
        PromotionGoods promotionGoods = new PromotionGoods();
        promotionGoods.setTitle("秒杀活动");
        promotionGoods.setSkuId(seckillApply.getSkuId());
        promotionGoods.setPromotionType(PromotionTypeEnum.SECKILL.name());
        promotionGoods.setPromotionId(seckillApply.getSeckillId());
        promotionGoods.setPrice(seckillApply.getPrice());
        promotionGoods.setNum(seckillApply.getQuantity());
        promotionGoods.setStoreId(seckillApply.getStoreId());
        promotionGoods.setPromotionStatus(PromotionStatusEnum.NEW.name());
        //商品活动的开始时间为当前商品的参加时间段
        int timeLine = seckillApply.getTimeLine();
        String date = cn.lili.common.utils.DateUtil.toString(seckillStartTime, cn.lili.common.utils.DateUtil.STANDARD_DATE_FORMAT);
        long startTime = cn.lili.common.utils.DateUtil.getDateline(date + " " + timeLine + ":00:00", cn.lili.common.utils.DateUtil.STANDARD_FORMAT);
        long endTime = cn.lili.common.utils.DateUtil.getDateline(date + " 23:59:59", cn.lili.common.utils.DateUtil.STANDARD_FORMAT);

        promotionGoods.setStartTime(new Date(startTime));
        promotionGoods.setEndTime(new Date(endTime));
        return promotionGoods;
    }

    /**
     * 检查缓存中是否存在相同商品参与的秒杀活动活动
     *
     * @param startTime 秒杀活动开始时间
     */
    private void checkCache(Long startTime) {
        String seckillCacheKey = PromotionCacheKeys.getSeckillTimelineKey(cn.lili.common.utils.DateUtil.toString(startTime, cn.lili.common.utils.DateUtil.STANDARD_DATE_NO_UNDERLINE_FORMAT));
        Map<Object, Object> hash = cache.getHash(seckillCacheKey);
        //如果缓存中存在当前审核商品参与的秒杀活动活动商品信息，清除
        if (hash != null && !hash.isEmpty()) {
            cache.remove(seckillCacheKey);
        }
    }

    /**
     * 从缓存中获取秒杀活动信息
     *
     * @param seckillCacheKey 秒杀活动缓存键
     * @return 秒杀活动信息
     */
    private List<SeckillTimelineVO> getSeckillTimelineToCache(String seckillCacheKey) {
        List<SeckillTimelineVO> timelineList = new ArrayList<>();
        LambdaQueryWrapper<Seckill> queryWrapper = new LambdaQueryWrapper<>();
        //查询当天时间段内的且状态不为结束或关闭的秒杀活动活动
        queryWrapper.gt(Seckill::getStartTime, new Date(cn.lili.common.utils.DateUtil.startOfTodDay() * 1000)).lt(Seckill::getEndTime, cn.lili.common.utils.DateUtil.endOfDate())
                .and(i -> i.eq(Seckill::getPromotionStatus, PromotionStatusEnum.NEW.name())
                        .or(j -> j.eq(Seckill::getPromotionStatus, PromotionStatusEnum.START.name())));
        List<Seckill> seckillList = seckillService.list(queryWrapper);
        if (!seckillList.isEmpty()) {
            for (Seckill seckill : seckillList) {
                //读取系统时间的时刻
                Calendar c = Calendar.getInstance();
                int hour = c.get(Calendar.HOUR_OF_DAY);
                String[] split = seckill.getHours().split(",");
                int[] hoursSored = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
                Arrays.sort(hoursSored);
                for (int i = 0; i < hoursSored.length; i++) {
                    SeckillTimelineVO tempTimeline = new SeckillTimelineVO();
                    boolean hoursSoredHour = (hoursSored[i] >= hour || ((i + 1) < hoursSored.length && hoursSored[i + 1] > hour));
                    if (hoursSoredHour) {
                        SimpleDateFormat format = new SimpleDateFormat(cn.lili.common.utils.DateUtil.STANDARD_DATE_FORMAT);
                        String date = format.format(new Date());
                        //当前时间的秒数
                        long currentTime = cn.lili.common.utils.DateUtil.getDateline();
                        //秒杀活动的时刻
                        long timeLine = cn.lili.common.utils.DateUtil.getDateline(date + " " + hoursSored[i], "yyyy-MM-dd HH");
                        if ((i + 1) < hoursSored.length && hour > hoursSored[i] && hour <= hoursSored[i + 1]) {
                            timeLine = cn.lili.common.utils.DateUtil.getDateline(date + " " + hoursSored[i + 1], "yyyy-MM-dd HH");
                        }
                        Long distanceTime = timeLine - currentTime < 0 ? 0 : timeLine - currentTime;
                        tempTimeline.setDistanceStartTime(distanceTime);
                        tempTimeline.setStartTime(timeLine);
                        tempTimeline.setTimeLine(hoursSored[i]);
                        tempTimeline.setSeckillGoodsList(wrapperSeckillGoods(hoursSored[i]));
                        timelineList.add(tempTimeline);
                    }
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
    private List<SeckillGoodsVO> wrapperSeckillGoods(Integer startTimeline) {
        List<SeckillGoodsVO> seckillGoodsVoS = new ArrayList<>();
        LambdaQueryWrapper<Seckill> seckillLambdaQueryWrapper = new LambdaQueryWrapper<>();
        seckillLambdaQueryWrapper.gt(Seckill::getStartTime, new Date(cn.lili.common.utils.DateUtil.startOfTodDay() * 1000)).lt(Seckill::getEndTime, cn.lili.common.utils.DateUtil.endOfDate())
                .and(i -> i.eq(Seckill::getPromotionStatus, PromotionStatusEnum.NEW.name())
                        .or(j -> j.eq(Seckill::getPromotionStatus, PromotionStatusEnum.START.name())));
        List<Seckill> seckillList = this.seckillService.list(seckillLambdaQueryWrapper);
        if (!seckillList.isEmpty()) {
            for (Seckill seckill : seckillList) {
                LambdaQueryWrapper<SeckillApply> seckillApplyLambdaQueryWrapper = new LambdaQueryWrapper<SeckillApply>().eq(SeckillApply::getTimeLine, startTimeline).eq(SeckillApply::getSeckillId, seckill.getId()).eq(SeckillApply::getPromotionApplyStatus, PromotionApplyStatusEnum.PASS.name());
                List<SeckillApply> list = this.list(seckillApplyLambdaQueryWrapper);
                for (SeckillApply seckillApply : list) {
                    GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(seckillApply.getSkuId());
                    if (goodsSku != null) {
                        SeckillGoodsVO goodsVO = new SeckillGoodsVO();
                        BeanUtil.copyProperties(seckillApply, goodsVO);
                        goodsVO.setGoodsImage(goodsSku.getThumbnail());
                        goodsVO.setGoodsId(goodsSku.getGoodsId());
                        goodsVO.setGoodsName(goodsSku.getGoodsName());
                        seckillGoodsVoS.add(goodsVO);
                    }
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
    private void checkSeckillGoodsSku(SeckillVO seckill, SeckillApplyVO seckillApply, GoodsSku goodsSku, DateTime startTime) {
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
        int nextHour = 23;
        String[] split = seckill.getHours().split(",");
        int[] hoursSored = Arrays.stream(split).mapToInt(Integer::parseInt).toArray();
        Arrays.sort(hoursSored);
        for (int i : hoursSored) {
            if (seckillApply.getTimeLine() < i) {
                nextHour = i;
                break;
            }
        }
        DateTime startTime = DateUtil.offsetHour(seckill.getStartTime(), seckillApply.getTimeLine());
        DateTime parseEndTime = DateUtil.offsetSecond(DateUtil.offsetHour(seckill.getStartTime(), nextHour), 1);
        promotionGoods.setStartTime(startTime);
        promotionGoods.setEndTime(parseEndTime);
        return promotionGoods;
    }
}