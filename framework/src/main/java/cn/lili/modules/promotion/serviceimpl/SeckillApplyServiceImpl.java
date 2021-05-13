package cn.lili.modules.promotion.serviceimpl;

import cn.hutool.core.bean.BeanUtil;
import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.date.DateTime;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.DateUtil;
import cn.lili.common.utils.PageUtil;
import cn.lili.common.utils.StringUtils;
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
import cn.lili.modules.search.service.EsGoodsIndexService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
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

    //缓存
    @Autowired
    private Cache<Object> cache;
    //Mongo
    @Autowired
    private MongoTemplate mongoTemplate;
    //规格商品
    @Autowired
    private GoodsSkuService goodsSkuService;
    //ES商品
    @Autowired
    private EsGoodsIndexService esGoodsIndexService;
    //促销商品
    @Autowired
    private PromotionGoodsService promotionGoodsService;
    //秒杀
    @Autowired
    private SeckillService seckillService;

    @Override
    public List<SeckillTimelineVO> getSeckillTimeline() {
        List<SeckillTimelineVO> timelineVoS = new ArrayList<>();
        // 限时抢购缓存key
        String seckillCacheKey = PromotionCacheKeys.getSeckillTimelineKey(DateUtil.toString(DateUtil.startOfTodDay(), DateUtil.STANDARD_DATE_NO_UNDERLINE_FORMAT));
        Map<Object, Object> cacheSeckill = cache.getHash(seckillCacheKey);
        if (cacheSeckill == null || cacheSeckill.isEmpty()) {
            // 如缓存中不存在，则单独获取
            try {
                timelineVoS = getSeckillTimelineToCache(seckillCacheKey);
            } catch (Exception e) {
                log.error("获取限时抢购信息发生错误！", e);
            }
        } else {
            // 如缓存中存在，则取缓存中转为展示的信息
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
        // 限时抢购缓存key
        String seckillCacheKey = PromotionCacheKeys.getSeckillTimelineKey(DateUtil.toString(DateUtil.startOfTodDay(), DateUtil.STANDARD_DATE_NO_UNDERLINE_FORMAT));
        Map<Object, Object> cacheSeckill = cache.getHash(seckillCacheKey);
        if (cacheSeckill == null || cacheSeckill.isEmpty()) {
            // 如缓存中不存在，则单独获取
            seckillGoodsVoS = wrapperSeckillGoods(timeline);
        } else {
            // 如缓存中存在，则取缓存中转为展示的信息
            for (Map.Entry<Object, Object> entry : cacheSeckill.entrySet()) {
                Integer timelineKey = Integer.parseInt(entry.getKey().toString());
                if (timelineKey.equals(timeline)) {
                    seckillGoodsVoS = (List<SeckillGoodsVO>) entry.getValue();
                }
            }
        }
        return seckillGoodsVoS;
    }

    @Override
    public void auditBatchApply(String[] ids, String seckillId, String applyStatus, String failReason) {
        if (ids == null || ids.length <= 0) {
            throw new ServiceException("请提供要审核的商品");
        }
        if (StringUtils.isEmpty(applyStatus) || PromotionApplyStatusEnum.valueOf(applyStatus).name().isEmpty()) {
            throw new ServiceException("提供的审批状态值不正确");
        }
        if (PromotionApplyStatusEnum.REFUSE.name().equals(applyStatus)) {
            boolean isEmpty = StringUtils.isEmpty(failReason);
            if (isEmpty) {
                throw new ServiceException("在驳回状态下驳回原因必填");
            }
        }

        SeckillVO seckillVO = this.mongoTemplate.findById(seckillId, SeckillVO.class);
        if (seckillVO == null) {
            log.error("编号为【" + seckillId + "】限时抢购请不存在");
            throw new ServiceException();
        }
        List<SeckillApply> seckillApplyList = seckillVO.getSeckillApplyList();

        for (String id : ids) {

            Optional<SeckillApply> seckillApplyOptional = seckillApplyList.stream().filter(i -> i.getId().equals(id)).findFirst();
            SeckillApply seckillApply;
            if (seckillApplyOptional.isPresent()) {
                seckillApply = seckillApplyOptional.get();
            } else {
                log.error("编号为【" + id + "】限时抢购的申请不存在");
                throw new ServiceException();
            }

            seckillApply.setPromotionApplyStatus(PromotionApplyStatusEnum.valueOf(applyStatus).name());
            seckillApply.setFailReason(failReason);
            this.updateById(seckillApply);
            if (PromotionApplyStatusEnum.PASS.name().equals(applyStatus)) {
                //检查缓存中是否存在相同商品参与的限时抢购活动
                checkCache(seckillVO.getStartTime().getTime());
            }
        }
        seckillVO.setSeckillApplyList(seckillApplyList);
        mongoTemplate.save(seckillVO);
    }

    @Override
    public IPage<SeckillApply> getSeckillApplyFromMysql(SeckillSearchParams queryParam, PageVO pageVo) {
        QueryWrapper<SeckillApply> queryWrapper = queryParam.wrapper();
        return page(PageUtil.initPage(pageVo), queryWrapper);
    }

    @Override
    public IPage<SeckillApply> getSeckillApplyFromMongo(SeckillSearchParams queryParam, PageVO pageVo) {
        IPage<SeckillApply> seckillApplyIPage = new Page<>();
        Query query = queryParam.mongoQuery();

        SeckillVO seckillVO = this.mongoTemplate.findOne(query, SeckillVO.class);
        if (seckillVO != null && pageVo != null) {
            seckillApplyIPage.setCurrent(pageVo.getMongoPageNumber());
            seckillApplyIPage.setSize(pageVo.getPageSize());
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
            seckillApplyIPage.setTotal(seckillApplyList.size());
            List<SeckillApply> page = CollUtil.page(pageVo.getMongoPageNumber(), pageVo.getPageSize(), seckillApplyList);
            seckillApplyIPage.setRecords(page);
            return seckillApplyIPage;
        } else {
            return null;
        }
    }

    @Override
    public void addSeckillApply(String seckillId, String storeId, List<SeckillApplyVO> seckillApplyList) {
        SeckillVO seckill = mongoTemplate.findById(seckillId, SeckillVO.class);
        if (seckill == null) {
            throw new ServiceException("当前参与的限时抢购不存在！");
        }
        seckill.checkTime();
        // 检查限时抢购申请是否合法
        checkSeckillApplyList(seckill.getApplyEndTime().getTime(), seckill.getHours(), seckillApplyList, storeId);
        String storeIds = seckill.getStoreIds() != null ? seckill.getStoreIds() : "";
        boolean containsStore = false;
        List<String> storeIdList = Arrays.asList(storeIds.split(","));
        // 检查是否为已参加活动的店铺
        if (CharSequenceUtil.isNotEmpty(seckillId) && !storeIdList.contains(storeId)) {
            if (!CharSequenceUtil.isEmpty(storeIds)) {
                String[] storeIdSplit = storeIds.split(",");
                for (String s : storeIdSplit) {
                    if (s.equals(seckillId)) {
                        containsStore = true;
                        break;
                    }
                }
                storeIds = seckill.getStoreIds() + storeId + ",";
            } else {
                storeIds = storeId + ",";
            }
            seckill.setStoreIds(storeIds);
        }

        List<SeckillApply> originList = seckill.getSeckillApplyList();
        List<PromotionGoods> promotionGoodsList = new ArrayList<>();
        if (originList == null) {
            originList = new ArrayList<>();
        }
        for (SeckillApplyVO seckillApply : seckillApplyList) {
            GoodsSku goodsSku = goodsSkuService.getGoodsSkuByIdFromCache(seckillApply.getSkuId());
            if (goodsSku.getQuantity() < seckillApply.getQuantity()) {
                throw new ServiceException(seckillApply.getGoodsName() + ",此商品库存不足");
            }

            /*
              *************两种情况：******************
              团购时间段：      |________________|
              秒杀时间段：  |_____|           |_______|

              ************第三种情况：******************
              团购时间段：        |______|
              秒杀时间段：   |________________|

              ************第四种情况：******************
              团购时间段：   |________________|
              秒杀时间段：        |______|

              这个商品的开始时间计算要用他参与的时间段来计算，结束时间是当天晚上23：59：59
             */
            String startTimeStr = DateUtil.toString(seckill.getStartTime(), DateUtil.STANDARD_DATE_FORMAT) + " " + (seckillApply.getTimeLine() > 10 ? ("0" + seckillApply.getTimeLine()) : seckillApply.getTimeLine()) + ":00:00";
            String endTimeStr = DateUtil.toString(seckill.getStartTime(), "yyyy-MM-dd") + " 23:59:59";

            // 查询是否在同一时间段参与了拼团活动
            Integer count = promotionGoodsService.findInnerOverlapPromotionGoods(PromotionTypeEnum.PINTUAN.name(), goodsSku.getId(), DateUtil.toDate(startTimeStr, DateUtil.STANDARD_FORMAT), DateUtil.toDate(endTimeStr, DateUtil.STANDARD_FORMAT));
            // 查询是否在同一时间段参与了限时抢购活动
            count += promotionGoodsService.findInnerOverlapPromotionGoods(PromotionTypeEnum.SECKILL.name(), goodsSku.getId(), DateUtil.toDate(startTimeStr, DateUtil.STANDARD_FORMAT), DateUtil.toDate(endTimeStr, DateUtil.STANDARD_FORMAT));
            if (count > 0) {
                throw new ServiceException("商品[" + goodsSku.getGoodsName() + "]已经在重叠的时间段参加了团购或限时抢购活动，不能参加限时抢购活动");
            }
            seckillApply.setOriginalPrice(goodsSku.getPrice());
            seckillApply.setPromotionApplyStatus(PromotionApplyStatusEnum.APPLY.name());
            seckillApply.setSalesNum(0);

            Optional<SeckillApply> first = originList.stream().filter(i -> i.getSkuId().equals(seckillApply.getSkuId())).findFirst();
            if (first.isPresent() && (first.get().getPromotionApplyStatus().equals(PromotionApplyStatusEnum.REFUSE.name()) || first.get().getPromotionApplyStatus().equals(PromotionApplyStatusEnum.APPLY.name()))) {
                originList = originList.stream().filter(i -> !i.getSkuId().equals(seckillApply.getSkuId())).collect(Collectors.toList());
            } else if (first.isPresent() && first.get().getPromotionApplyStatus().equals(PromotionApplyStatusEnum.PASS.name())) {
                continue;
            }
            originList.add(seckillApply);
            PromotionGoods promotionGoods = new PromotionGoods(goodsSku);
            promotionGoods.setPrice(seckillApply.getPrice());
            promotionGoods.setQuantity(seckillApply.getQuantity());
            // 设置单独每个促销商品的结束时间
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
            String format = cn.hutool.core.date.DateUtil.format(seckill.getStartTime(), DateUtil.STANDARD_DATE_FORMAT);
            DateTime parseStartTime = cn.hutool.core.date.DateUtil.parse((format + " " + seckillApply.getTimeLine()), "yyyy-MM-dd HH");
            DateTime parseEndTime = cn.hutool.core.date.DateUtil.parse((format + " " + nextHour), "yyyy-MM-dd HH");
            // 如果是当天最后的时间段则设置到当天结束时间的59分59秒
            if (nextHour == seckillApply.getTimeLine()) {
                parseEndTime = cn.hutool.core.date.DateUtil.parse((format + " " + nextHour + ":59:59"), DateUtil.STANDARD_FORMAT);
            }
            promotionGoods.setStartTime(parseStartTime);
            promotionGoods.setEndTime(parseEndTime);
            promotionGoodsList.add(promotionGoods);
        }
        this.saveOrUpdateBatch(originList);
        seckill.setSeckillApplyList(originList);
        this.mongoTemplate.save(seckill);
        if (!promotionGoodsList.isEmpty()) {
            LambdaQueryWrapper<PromotionGoods> queryWrapper = new LambdaQueryWrapper<>();
            queryWrapper.in(PromotionGoods::getSkuId, promotionGoodsList.stream().map(PromotionGoods::getSkuId).collect(Collectors.toList())).eq(PromotionGoods::getStoreId, storeId);
            promotionGoodsService.remove(queryWrapper);
            PromotionTools.promotionGoodsInit(promotionGoodsList, seckill, PromotionTypeEnum.SECKILL);
            promotionGoodsService.saveBatch(promotionGoodsList);
        }

        if (Boolean.FALSE.equals(containsStore)) {
            seckillService.storeApply(storeId, seckill.getId());
        }
    }

    /**
     * 批量删除限时抢购申请
     *
     * @param seckillId 限时抢购活动id
     * @param ids       限时抢购申请id集合
     */
    @Override
    public void removeSeckillApplyByIds(String seckillId, List<String> ids) {
        SeckillVO seckillVO = this.mongoTemplate.findById(seckillId, SeckillVO.class);
        if (seckillVO == null) {
            throw new ServiceException("当前限时抢购活动不存在！");
        }
        if (seckillVO.getPromotionStatus().equals(PromotionStatusEnum.START.name())) {
            throw new ServiceException("当前限时抢购活动已经开始，无法修改！");
        }
        seckillVO.getSeckillApplyList().removeIf(seckillApply -> ids.contains(seckillApply.getId()));
        this.mongoTemplate.save(seckillVO);
        this.removeByIds(ids);
    }

    /**
     * 更新限时抢购库存数量
     *
     * @param id  限时抢购申请（限时抢购商品）id
     * @param num 数量
     * @return 是否成功
     */
    @Override
    public boolean updateSeckillStock(String id, Integer num) {
        List<String> promotionIds = this.esGoodsIndexService.getPromotionIdByPromotionType(id, PromotionTypeEnum.SECKILL);
        for (String promotionId : promotionIds) {
            SeckillVO seckillVO = this.mongoTemplate.findById(promotionId, SeckillVO.class);
            if (seckillVO == null) {
                log.error("限时抢购活动id为" + promotionId + "的限时抢购活动不存在！");
                break;
            }
            if (seckillVO.getSeckillApplyList() != null && !seckillVO.getSeckillApplyList().isEmpty()) {
                Optional<SeckillApply> seckillApplyOptional = seckillVO.getSeckillApplyList().stream().filter(i -> i.getSkuId().equals(id)).findFirst();
                if (seckillApplyOptional.isPresent()) {
                    SeckillApply seckillApply = seckillApplyOptional.get();
                    // 设置售卖数量
                    Integer countNum = seckillApply.getSalesNum() + num;
                    seckillApply.setSalesNum(countNum);
                    LambdaUpdateWrapper<SeckillApply> updateWrapper = new LambdaUpdateWrapper<>();
                    updateWrapper.eq(SeckillApply::getId, seckillApply.getId());
                    updateWrapper.set(SeckillApply::getQuantity, seckillApply.getQuantity() - num);
                    updateWrapper.set(SeckillApply::getSalesNum, countNum);
                    this.update(updateWrapper);
                    this.mongoTemplate.save(seckillVO);
                }
            }
        }
        return true;
    }

    /**
     * 更新限时抢购库存数量
     *
     * @param map key 为 限时抢购申请（限时抢购商品）id， value 为数量
     * @return 是否成功
     */
    @Override
    public boolean updateSeckillStock(Map<String, Integer> map) {
        boolean result = false;
        for (Map.Entry<String, Integer> entry : map.entrySet()) {
            result = this.updateSeckillStock(entry.getKey(), entry.getValue());
        }
        return result;
    }

    /**
     * 检查限时抢购申请列表参数信息
     *
     * @param applyEndTime     申请结束时间
     * @param hours            限时抢购时间段
     * @param seckillApplyList 限时抢购申请列表
     * @param storeId          当前申请商家编号
     */
    private void checkSeckillApplyList(Long applyEndTime, String hours, List<SeckillApplyVO> seckillApplyList, String storeId) {
        List<String> existSku = new ArrayList<>();
        for (SeckillApplyVO seckillApply : seckillApplyList) {
            seckillApply.setStoreId(storeId);
            if (seckillApply.getPrice() > seckillApply.getOriginalPrice()) {
                throw new ServiceException("活动价格不能大于商品原价");
            }

            // 检查限时抢购申请的时刻，是否存在在限时抢购的时间段内
            String[] rangeHours = hours.split(",");
            boolean containsSame = Arrays.stream(rangeHours).anyMatch(i -> i.equals(seckillApply.getTimeLine().toString()));
            if (!containsSame) {
                throw new ServiceException("时刻参数异常");
            }

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
     * @param seckillApply     限时抢购申请信息
     * @param seckillStartTime 当前限时抢购申请的开始时间
     * @return 促销商品信息
     */
    private PromotionGoods setPromotionGoods(SeckillApply seckillApply, Date seckillStartTime) {
        PromotionGoods promotionGoods = new PromotionGoods();
        promotionGoods.setTitle("限时抢购");
        promotionGoods.setSkuId(seckillApply.getSkuId());
        promotionGoods.setPromotionType(PromotionTypeEnum.SECKILL.name());
        promotionGoods.setPromotionId(seckillApply.getSeckillId());
        promotionGoods.setPrice(seckillApply.getPrice());
        promotionGoods.setNum(seckillApply.getQuantity());
        promotionGoods.setStoreId(seckillApply.getStoreId());
        promotionGoods.setPromotionStatus(PromotionStatusEnum.NEW.name());
        //商品活动的开始时间为当前商品的参加时间段
        int timeLine = seckillApply.getTimeLine();
        String date = DateUtil.toString(seckillStartTime, DateUtil.STANDARD_DATE_FORMAT);
        long startTime = DateUtil.getDateline(date + " " + timeLine + ":00:00", DateUtil.STANDARD_FORMAT);
        long endTime = DateUtil.getDateline(date + " 23:59:59", DateUtil.STANDARD_FORMAT);

        promotionGoods.setStartTime(new Date(startTime));
        promotionGoods.setEndTime(new Date(endTime));
        return promotionGoods;
    }

    /**
     * 检查缓存中是否存在相同商品参与的限时抢购活动
     *
     * @param startTime 限时抢购开始时间
     */
    private void checkCache(Long startTime) {
        String seckillCacheKey = PromotionCacheKeys.getSeckillTimelineKey(DateUtil.toString(startTime, DateUtil.STANDARD_DATE_NO_UNDERLINE_FORMAT));
        Map<Object, Object> hash = cache.getHash(seckillCacheKey);
        //如果缓存中存在当前审核商品参与的限时抢购活动商品信息，清除
        if (hash != null && !hash.isEmpty()) {
            cache.remove(seckillCacheKey);
        }
    }

    /**
     * 从缓存中获取限时抢购信息
     *
     * @param seckillCacheKey 限时抢购缓存键
     * @return 限时抢购信息
     */
    private List<SeckillTimelineVO> getSeckillTimelineToCache(String seckillCacheKey) {
        List<SeckillTimelineVO> timelineList = new ArrayList<>();
        LambdaQueryWrapper<Seckill> queryWrapper = new LambdaQueryWrapper<>();
        // 查询当天时间段内的且状态不为结束或关闭的限时抢购活动
        queryWrapper.gt(Seckill::getStartTime, new Date(DateUtil.startOfTodDay() * 1000)).lt(Seckill::getEndTime, DateUtil.endOfDate())
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
                    if (hoursSored[i] >= hour || ((i + 1) < hoursSored.length && hoursSored[i + 1] > hour)) {
                        SimpleDateFormat format = new SimpleDateFormat(DateUtil.STANDARD_DATE_FORMAT);
                        String date = format.format(new Date());
                        //当前时间的秒数
                        long currentTime = DateUtil.getDateline();
                        //限时抢购的时刻
                        long timeLine = DateUtil.getDateline(date + " " + hoursSored[i], "yyyy-MM-dd HH");
                        if ((i + 1) < hoursSored.length && hour > hoursSored[i] && hour <= hoursSored[i + 1]) {
                            timeLine = DateUtil.getDateline(date + " " + hoursSored[i + 1], "yyyy-MM-dd HH");
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
     * 组装当时间限时抢购的商品数据
     * w
     *
     * @param startTimeline 限时抢购开始时刻
     * @return 当时间限时抢购的商品数据
     */
    private List<SeckillGoodsVO> wrapperSeckillGoods(Integer startTimeline) {
        List<SeckillGoodsVO> seckillGoodsVoS = new ArrayList<>();
        LambdaQueryWrapper<Seckill> seckillLambdaQueryWrapper = new LambdaQueryWrapper<>();
        seckillLambdaQueryWrapper.gt(Seckill::getStartTime, new Date(DateUtil.startOfTodDay() * 1000)).lt(Seckill::getEndTime, DateUtil.endOfDate())
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
}