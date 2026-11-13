package cn.lili.modules.promotion.serviceimpl;


import cn.hutool.core.convert.Convert;
import cn.hutool.core.util.RandomUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityGoods;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityDTO;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityQuery;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.enums.KanJiaStatusEnum;
import cn.lili.modules.promotion.entity.enums.PromotionsStatusEnum;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityVO;
import cn.lili.modules.promotion.mapper.KanJiaActivityMapper;
import cn.lili.modules.promotion.service.KanjiaActivityGoodsService;
import cn.lili.modules.promotion.service.KanjiaActivityLogService;
import cn.lili.modules.promotion.service.KanjiaActivityService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.transaction.support.TransactionTemplate;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Objects;
import java.util.concurrent.TimeUnit;
import java.util.function.Supplier;


/**
 * 砍价活动参与记录业务层实现
 * 单向调用日志服务完成砍价记录写入；日志服务不回调本服务，无需延迟注入。
 *
 * @author qiuqiu
 * @since 2021/7/1
 */
@Service
public class KanjiaActivityServiceImpl extends ServiceImpl<KanJiaActivityMapper, KanjiaActivity> implements KanjiaActivityService {

    @Autowired
    private KanjiaActivityGoodsService kanjiaActivityGoodsService;
    @Autowired
    private KanjiaActivityLogService kanjiaActivityLogService;
    @Autowired
    private MemberService memberService;
    @Autowired
    private GoodsSkuService goodsSkuService;
    @Autowired
    private RedissonClient redissonClient;
    @Autowired
    private PlatformTransactionManager transactionManager;

    @Override
    public KanjiaActivity getKanjiaActivity(KanjiaActivitySearchParams kanJiaActivitySearchParams) {
        return this.getOne(kanJiaActivitySearchParams.wrapper());
    }

    @Override
    public KanjiaActivityVO getKanjiaActivityVO(KanjiaActivitySearchParams kanJiaActivitySearchParams) {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        KanjiaActivity kanjiaActivity = this.getKanjiaActivity(kanJiaActivitySearchParams);
        KanjiaActivityVO kanjiaActivityVO = new KanjiaActivityVO();
        //判断是否参与活动
        if (kanjiaActivity == null) {
            return kanjiaActivityVO;
        }
        BeanUtil.copyProperties(kanjiaActivity, kanjiaActivityVO);

        //判断是否发起了砍价活动,如果发起可参与活动
        kanjiaActivityVO.setLaunch(true);
        //如果已发起砍价判断用户是否可以砍价
        KanjiaActivityLog kanjiaActivityLog = kanjiaActivityLogService.getOne(new LambdaQueryWrapper<KanjiaActivityLog>()
                .eq(KanjiaActivityLog::getKanjiaActivityId, kanjiaActivity.getId())
                .eq(KanjiaActivityLog::getKanjiaMemberId, authUser.getId()));
        if (kanjiaActivityLog == null) {
            kanjiaActivityVO.setHelp(true);
        }
        //判断活动已通过并且是当前用户发起的砍价则可以进行购买
        if (kanjiaActivity.getStatus().equals(KanJiaStatusEnum.SUCCESS.name()) &&
                kanjiaActivity.getMemberId().equals(UserContext.getCurrentUser().getId())) {
            kanjiaActivityVO.setPass(true);
        }
        return kanjiaActivityVO;
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public KanjiaActivityLog add(String id) {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        //根据skuId查询当前sku是否参与活动并且是在活动进行中
        KanjiaActivityGoods kanJiaActivityGoods = kanjiaActivityGoodsService.getById(id);
        //只有砍价商品存在且已经开始的活动才可以发起砍价
        if (kanJiaActivityGoods == null || !kanJiaActivityGoods.getPromotionStatus().equals(PromotionsStatusEnum.START.name())) {
            throw new ServiceException(ResultCode.PROMOTION_STATUS_END);
        }
        KanjiaActivityLog kanjiaActivityLog = new KanjiaActivityLog();
        //获取客户信息
        Member member = memberService.getById(authUser.getId());
        //校验此活动是否已经发起过
        QueryWrapper<KanjiaActivity> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq("kanjia_activity_goods_id", kanJiaActivityGoods.getId());
        queryWrapper.eq("member_id", member.getId());
        if (this.count(queryWrapper) > 0) {
            throw new ServiceException(ResultCode.KANJIA_ACTIVITY_MEMBER_ERROR);
        }
        KanjiaActivity kanJiaActivity = new KanjiaActivity();
        //获取商品信息
        GoodsSku goodsSku = goodsSkuService.getCanPromotionGoodsSkuByIdFromCache(kanJiaActivityGoods.getSkuId());
        if (goodsSku != null) {
            kanJiaActivity.setSkuId(kanJiaActivityGoods.getSkuId());
            kanJiaActivity.setGoodsName(goodsSku.getGoodsName());
            kanJiaActivity.setKanjiaActivityGoodsId(kanJiaActivityGoods.getId());
            kanJiaActivity.setThumbnail(goodsSku.getThumbnail());
            kanJiaActivity.setMemberId(UserContext.getCurrentUser().getId());
            kanJiaActivity.setMemberName(member.getUsername());
            kanJiaActivity.setStatus(KanJiaStatusEnum.START.name());
            //剩余砍价金额 开始 是商品金额
            kanJiaActivity.setSurplusPrice(goodsSku.getPrice());
            //砍价最低购买金额
            kanJiaActivity.setPurchasePrice(kanJiaActivityGoods.getPurchasePrice());
            //保存我的砍价活动
            boolean result = this.save(kanJiaActivity);

            //因为发起砍价就是自己给自己砍一刀，所以要添加砍价记录信息
            if (result) {
                kanjiaActivityLog = this.helpKanJia(kanJiaActivity.getId());
            }
        }
        return kanjiaActivityLog;
    }


    @Override
    public KanjiaActivityLog helpKanJia(String kanjiaActivityId) {
        AuthUser authUser = Objects.requireNonNull(UserContext.getCurrentUser());
        RLock lock = redissonClient.getLock("KANJIA_ACTIVITY_LOCK:" + kanjiaActivityId);
        boolean locked = false;
        try {
            // 等待3秒，leaseTime=-1启用看门狗自动续期，避免长时间业务执行锁提前失效
            locked = lock.tryLock(3, -1, TimeUnit.SECONDS);
            if (!locked) {
                throw new ServiceException(ResultCode.PROMOTION_LOG_EXIST);
            }

            return executeInTransaction(() -> {
                //获取客户信息
                Member member = memberService.getById(authUser.getId());
                //根据砍价发起活动id查询砍价活动信息
                KanjiaActivity kanjiaActivity = this.getById(kanjiaActivityId);
                //判断活动非空或非正在进行中的活动
                if (kanjiaActivity == null || !kanjiaActivity.getStatus().equals(PromotionsStatusEnum.START.name())) {
                    throw new ServiceException(ResultCode.PROMOTION_STATUS_END);
                } else if (member == null) {
                    throw new ServiceException(ResultCode.USER_NOT_EXIST);
                }
                //根据skuId查询当前sku是否参与活动并且是在活动进行中
                KanjiaActivityGoods kanJiaActivityGoods = kanjiaActivityGoodsService.getById(kanjiaActivity.getKanjiaActivityGoodsId());
                if (kanJiaActivityGoods == null) {
                    throw new ServiceException(ResultCode.PROMOTION_STATUS_END);
                }
                //判断是否已参与
                LambdaQueryWrapper<KanjiaActivityLog> lambdaQueryWrapper = new LambdaQueryWrapper<KanjiaActivityLog>()
                        .eq(KanjiaActivityLog::getKanjiaActivityId, kanjiaActivityId)
                        .eq(KanjiaActivityLog::getKanjiaMemberId, member.getId());
                if (kanjiaActivityLogService.count(lambdaQueryWrapper) > 0) {
                    throw new ServiceException(ResultCode.PROMOTION_LOG_EXIST);
                }

                //添加砍价记录
                KanjiaActivityDTO kanjiaActivityDTO = new KanjiaActivityDTO();
                kanjiaActivityDTO.setKanjiaActivityGoodsId(kanjiaActivity.getKanjiaActivityGoodsId());
                kanjiaActivityDTO.setKanjiaActivityId(kanjiaActivityId);
                //获取砍价金额
                Double price = this.getKanjiaPrice(kanJiaActivityGoods, kanjiaActivity.getSurplusPrice());
                kanjiaActivityDTO.setKanjiaPrice(price);
                //计算剩余金额
                kanjiaActivityDTO.setSurplusPrice(CurrencyUtil.sub(kanjiaActivity.getSurplusPrice(), price));
                kanjiaActivityDTO.setKanjiaMemberId(member.getId());
                kanjiaActivityDTO.setKanjiaMemberName(member.getUsername());
                kanjiaActivityDTO.setKanjiaMemberFace(member.getFace());
                KanjiaActivityLog kanjiaActivityLog = kanjiaActivityLogService.addKanJiaActivityLog(kanjiaActivityDTO);

                //如果可砍金额为0的话说明活动成功了
                if (Double.doubleToLongBits(kanjiaActivityDTO.getSurplusPrice()) == Double.doubleToLongBits(0D)) {
                    kanjiaActivity.setStatus(KanJiaStatusEnum.SUCCESS.name());
                }
                kanjiaActivity.setSurplusPrice(kanjiaActivityLog.getSurplusPrice());
                this.updateById(kanjiaActivity);
                return kanjiaActivityLog;
            });
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ServiceException(ResultCode.ERROR);
        } catch (DuplicateKeyException e) {
            // 数据库联合唯一索引兜底，当并发穿透时避免抛出500异常
            throw new ServiceException(ResultCode.PROMOTION_LOG_EXIST);
        } finally {
            if (locked && lock.isHeldByCurrentThread()) {
                lock.unlock();
            }
        }
    }



    @Override
    public IPage<KanjiaActivity> getForPage(KanjiaActivityQuery kanjiaActivityQuery, PageVO page) {
        QueryWrapper<KanjiaActivity> queryWrapper = kanjiaActivityQuery.wrapper();
        return this.page(PageUtil.initPage(page), queryWrapper);
    }

    /**
     * 结束砍价活动
     *
     * @param kanjiaId 砍价活动id
     * @return 是否更新成功
     */
    @Override
    public boolean endKanjiaActivity(String kanjiaId) {
        return this.update(new LambdaUpdateWrapper<KanjiaActivity>()
                .eq(KanjiaActivity::getId, kanjiaId)
                .set(KanjiaActivity::getStatus, KanJiaStatusEnum.END.name()));
    }

    /**
     * 随机获取砍一刀价格
     *
     * @param kanjiaActivityGoods 砍价商品信息
     * @param surplusPrice        剩余可砍金额
     * @return 砍一刀价格
     */
    private Double getKanjiaPrice(KanjiaActivityGoods kanjiaActivityGoods, Double surplusPrice) {

        //如果剩余砍价金额小于最低砍价金额则返回0
        if (kanjiaActivityGoods.getLowestPrice() > surplusPrice) {
            return surplusPrice;
        }

        //如果金额相等则直接返回
        if (kanjiaActivityGoods.getLowestPrice().equals(kanjiaActivityGoods.getHighestPrice())) {
            return kanjiaActivityGoods.getLowestPrice();
        }
        //获取随机砍价金额
        BigDecimal bigDecimal = RandomUtil.randomBigDecimal(Convert.toBigDecimal(kanjiaActivityGoods.getLowestPrice()),
                Convert.toBigDecimal(kanjiaActivityGoods.getHighestPrice()));

        if (bigDecimal.setScale(2, RoundingMode.UP).doubleValue() > surplusPrice) {
            return surplusPrice;
        }
        return bigDecimal.setScale(2, RoundingMode.UP).doubleValue();

    }

    /**
     * 在事务中执行操作；若无事务管理器则直接在当前上下文执行。
     *
     * @param action 事务内执行的逻辑
     * @param <T>    返回值类型
     * @return 业务执行结果
     */
    private <T> T executeInTransaction(Supplier<T> action) {
        if (transactionManager != null) {
            return new TransactionTemplate(transactionManager).execute(status -> action.get());
        }
        return action.get();
    }
}
