package cn.lili.modules.promotion.serviceimpl;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.*;
import cn.lili.modules.member.entity.enums.PointTypeEnum;
import cn.lili.modules.member.mapper.*;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.promotion.entity.dos.*;
import cn.lili.modules.promotion.entity.enums.MemberCouponStatusEnum;
import cn.lili.modules.promotion.mapper.*;
import cn.lili.modules.promotion.service.MemberCouponService;
import cn.lili.modules.promotion.service.LotteryService;
import cn.lili.modules.promotion.support.CouponReceiveSupport;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.stereotype.Service;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.transaction.annotation.Transactional;
import java.security.SecureRandom;
import java.time.*;
import java.util.*;

/**
 * 平台免费抽奖。按会员、活动、奖项、券模板顺序加锁，次数、奖品库存、发奖与流水共同提交。
 * 自然日采用北京时间；固定概率命中不可用奖品时落为谢谢参与，不重抽、不转移概率。
 * 发布后冻结奖池；同请求号始终返回原结果，失败回滚后可使用原请求号重试。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
@Service
public class LotteryServiceImpl implements LotteryService {
    private static final ZoneId ZONE = ZoneId.of("Asia/Shanghai");
    private final SecureRandom random = new SecureRandom();
    private final Clock clock;
    private final LotteryActivityMapper activities;
    private final LotteryPrizeMapper prizes;
    private final LotteryRecordMapper records;
    private final MemberMapper members;
    private final MemberService memberService;
    private final MemberPointsHistoryMapper histories;
    private final CouponMapper couponLocks;
    private final MemberCouponService memberCoupons;
    private final CouponReceiveSupport receive;

    /**
     * 注入业务存储和公共积分、发券能力。
     */
    @Autowired
    public LotteryServiceImpl(LotteryActivityMapper activities, LotteryPrizeMapper prizes, LotteryRecordMapper records,
            MemberMapper members, MemberService memberService, MemberPointsHistoryMapper histories,
            CouponMapper couponLocks, MemberCouponService memberCoupons, CouponReceiveSupport receive) {
        this(activities, prizes, records, members, memberService, histories, couponLocks, memberCoupons, receive, Clock.system(ZONE));
    }

    /**
     * 使用可控时钟验证活动边界与北京时间日切，生产构造器使用系统时钟。
     */
    LotteryServiceImpl(LotteryActivityMapper activities, LotteryPrizeMapper prizes, LotteryRecordMapper records,
            MemberMapper members, MemberService memberService, MemberPointsHistoryMapper histories,
            CouponMapper couponLocks, MemberCouponService memberCoupons, CouponReceiveSupport receive, Clock clock) {
        this.activities = activities; this.prizes = prizes; this.records = records; this.members = members;
        this.memberService = memberService; this.histories = histories; this.couponLocks = couponLocks;
        this.memberCoupons = memberCoupons; this.receive = receive;
        this.clock = clock;
    }

    /**
     * 查询活动与奖池；买家仅查询有效上架活动，不返回库存及内部关联字段。
     */
    @Override
    public IPage<LotteryActivity> activities(PageVO page, boolean buyer) {
        var query = new LambdaQueryWrapper<LotteryActivity>().orderByDesc(LotteryActivity::getCreateTime).orderByDesc(LotteryActivity::getId);
        Date current = now();
        if (buyer) query.eq(LotteryActivity::getEnabled, true).le(LotteryActivity::getStartTime, current).gt(LotteryActivity::getEndTime, current);
        return activities.selectPage(page(page), query).convert(activity -> {
            activity.setPrizes(prizes.selectList(new LambdaQueryWrapper<LotteryPrize>().eq(LotteryPrize::getActivityId, activity.getId()).orderByAsc(LotteryPrize::getSortOrder))
                .stream().map(prize -> {
                    if (!buyer) return prize;
                    LotteryPrize view = new LotteryPrize();
                    view.setId(prize.getId()); view.setName(prize.getName()); view.setType(prize.getType());
                    view.setPoints(prize.getPoints()); view.setSortOrder(prize.getSortOrder()); view.setWeight(prize.getWeight());
                    return view;
                }).toList());
            return activity;
        });
    }

    /**
     * 保存草稿；发布后不允许修改规则，防止历史次数和概率依据变化。
     *
     * @param input 草稿
     * @return 保存结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public LotteryActivity configure(LotteryActivity input) {
        require(input != null && text(input.getName(), 100) && input.getDescription() != null && input.getDescription().length() <= 2000,
                "请填写活动名称和规则说明（最多2000字）");
        require(input.getDailyLimit() != null && input.getDailyLimit() >= 1 && input.getDailyLimit() <= 100, "每日免费次数须为1至100");
        require(input.getStartTime() != null && input.getEndTime() != null && input.getStartTime().before(input.getEndTime()), "结束时间必须晚于开始时间");
        boolean creating = input.getId() == null || input.getId().isBlank();
        LotteryActivity row = creating ? new LotteryActivity() : activity(input.getId());
        require(!Boolean.TRUE.equals(row.getPublished()), "已发布活动不可修改规则，请新建活动");
        List<LotteryPrize> pool = input.getPrizes();
        require(pool != null && pool.size() >= 2 && pool.size() <= 8, "请配置2至8个奖项（含一个谢谢参与）");
        int weight = 0, thanks = 0;
        for (LotteryPrize prize : pool) {
            require(prize != null && text(prize.getName(), 100) && Set.of("POINTS", "COUPON", "THANKS").contains(Objects.requireNonNullElse(prize.getType(), "")), "奖项名称或类型不合法");
            require(validPercentage(prize.getWeight()), "百分比必须为1至100的正整数");
            weight += prize.getWeight();
            if ("THANKS".equals(prize.getType())) { thanks++; require(prize.getWeight() > 0, "谢谢参与概率必须大于0"); }
            else require(prize.getTotal() != null && prize.getTotal() > 0, "中奖奖项库存必须大于0");
            if ("POINTS".equals(prize.getType())) require(prize.getPoints() != null && prize.getPoints() > 0 && prize.getPoints() <= 1000000, "积分奖须为1至1000000整数积分");
        }
        require(weight == 10000 && thanks == 1, "奖项概率必须合计10000万分点，且恰好包含一个谢谢参与");
        // 多张券按ID顺序加锁，避免不同活动配置出现相反模板锁顺序。
        pool.stream().filter(p -> "COUPON".equals(p.getType())).map(LotteryPrize::getCouponId).peek(id -> require(text(id, 64), "请选择平台活动券"))
            .distinct().sorted().forEach(id -> require(validCoupon(couponLocks.lockCoupon(id), now(), false), "请选择仍有效的平台活动券"));
        row.setName(input.getName().trim()); row.setDescription(input.getDescription()); row.setDailyLimit(input.getDailyLimit());
        row.setStartTime(input.getStartTime()); row.setEndTime(input.getEndTime()); row.setEnabled(false); row.setPublished(false);
        require((creating ? activities.insert(row) : activities.updateById(row)) == 1, "活动保存失败");
        if (!creating) prizes.delete(new LambdaQueryWrapper<LotteryPrize>().eq(LotteryPrize::getActivityId, row.getId()));
        List<LotteryPrize> saved = new ArrayList<>();
        for (int i = 0; i < pool.size(); i++) {
            LotteryPrize source = pool.get(i), prize = new LotteryPrize();
            prize.setActivityId(row.getId()); prize.setName(source.getName().trim()); prize.setType(source.getType()); prize.setWeight(source.getWeight());
            prize.setTotal("THANKS".equals(prize.getType()) ? 0 : source.getTotal()); prize.setIssued(0); prize.setSortOrder(i);
            prize.setPoints("POINTS".equals(prize.getType()) ? source.getPoints() : 0L);
            prize.setCouponId("COUPON".equals(prize.getType()) ? source.getCouponId() : null);
            require(prizes.insert(prize) == 1, "奖项保存失败"); saved.add(prize);
        }
        row.setPrizes(saved); return row;
    }

    /**
     * 上下架保留库存及历史；首次发布冻结规则，过期不能再次上架。
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public void status(String id, boolean enabled) {
        LotteryActivity row = activity(id);
        if (enabled) {
            require(row.getEndTime().after(now()), "活动已结束");
            List<LotteryPrize> pool = prizes.lockPool(id);
            require(pool.size() >= 2 && pool.stream().mapToInt(LotteryPrize::getWeight).sum() == 10000, "奖池配置不完整");
            // 未发布草稿按新规则校验；已经发布的奖池保留原概率，不因上下架改变历史活动规则。
            if (!Boolean.TRUE.equals(row.getPublished())) require(pool.stream().allMatch(p -> validPercentage(p.getWeight())), "百分比必须为1至100的正整数");
            pool.stream().filter(p -> "COUPON".equals(p.getType())).map(LotteryPrize::getCouponId).distinct().sorted()
                .forEach(coupon -> require(validCoupon(couponLocks.lockCoupon(coupon), now(), false), "奖池优惠券已失效"));
            row.setPublished(true);
        }
        row.setEnabled(enabled); require(activities.updateById(row) == 1, "活动状态更新失败");
    }

    /**
     * 返回今日已用及剩余次数；仅供展示，实际资格由抽奖事务再次校验。
     */
    @Override
    public Map<String, Object> chances(String id, String member) {
        LotteryActivity row = activities.selectById(id); require(row != null, "抽奖活动不存在");
        LocalDate day = now().toInstant().atZone(ZONE).toLocalDate(); long used = used(id, member, day);
        return Map.of("day", day.toString(), "used", used, "remaining", Math.max(0, row.getDailyLimit() - used));
    }

    /**
     * 执行免费抽奖。会员锁保护每日次数，活动及奖项当前读保护库存；同请求跨天重试仍返回原结果。
     * 券不可领或奖品无库存返回谢谢参与；系统发奖错误整体回滚，不消耗次数，不留下中奖未发的记录。
     *
     * @param id 活动ID
     * @param memberId 当前会员ID
     * @param requestId 稳定请求号
     * @return 已完成结果
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public LotteryRecord draw(String id, String memberId, String requestId) {
        require(requestId != null && requestId.matches("[A-Za-z0-9-]{1,64}"), "抽奖请求号不合法");
        Member member = members.lockPointMember(memberId);
        require(member != null && Boolean.TRUE.equals(member.getDisabled()), "会员不存在或已禁用");
        LotteryRecord previous = records.findRequest(memberId, requestId);
        if (previous != null) { require(id.equals(previous.getActivityId()), "请求号已用于其他活动"); return previous; }
        LotteryActivity row = activity(id); Date now = now();
        require(Boolean.TRUE.equals(row.getEnabled()) && !now.before(row.getStartTime()) && now.before(row.getEndTime()), "活动未开始、已结束或已下架");
        LocalDate day = now.toInstant().atZone(ZONE).toLocalDate();
        require(used(id, memberId, day) < row.getDailyLimit(), "今日免费次数已用完");
        List<LotteryPrize> pool = prizes.lockPool(id);
        LotteryPrize selected = select(pool, random.nextInt(10000));
        LotteryPrize thanks = pool.stream().filter(p -> "THANKS".equals(p.getType())).findFirst().orElseThrow(() -> error("奖池配置不完整"));
        if (!"THANKS".equals(selected.getType()) && selected.getIssued() >= selected.getTotal()) selected = thanks;
        Coupon coupon = null;
        if ("COUPON".equals(selected.getType())) {
            coupon = couponLocks.lockCoupon(selected.getCouponId());
            if (!validCoupon(coupon, now, true)) selected = thanks;
            else {
                // 仅资格/发行量不足可落为未中奖，数据库及系统异常继续抛出以回滚。
                try { memberCoupons.checkCouponLimit(coupon.getId(), memberId); }
                catch (ServiceException ex) {
                    if (!Set.of(ResultCode.COUPON_RECEIVE_ERROR, ResultCode.COUPON_NUM_INSUFFICIENT_ERROR, ResultCode.COUPON_LIMIT_ERROR).contains(ex.getResultCode())) throw ex;
                    selected = thanks;
                }
            }
        }
        LotteryRecord result = new LotteryRecord();
        result.setActivityId(id); result.setActivityName(row.getName()); result.setMemberId(memberId); result.setRequestId(requestId);
        result.setDrawDay(day); result.setPrizeId(selected.getId()); result.setPrizeName(selected.getName()); result.setPrizeType(selected.getType()); result.setPoints(selected.getPoints());
        if ("POINTS".equals(selected.getType())) awardPoints(member, result);
        if ("COUPON".equals(selected.getType())) {
            receive.receiveCoupon(coupon.getId(), 1);
            MemberCoupon issued = new MemberCoupon(coupon); issued.setMemberId(memberId); issued.setMemberName(member.getUsername());
            // 动态券从中奖时刻生效，按北京时间包含中奖当日的N个自然日至末日23:59:59.999，避免依赖主机时区。
            if ("DYNAMICTIME".equals(coupon.getRangeDayType())) {
                issued.setStartTime(now);
                issued.setEndTime(Date.from(day.plusDays(coupon.getEffectiveDays()).atStartOfDay(ZONE).toInstant().minusMillis(1)));
            }
            issued.setPlatformFlag(true); issued.setMemberCouponStatus(MemberCouponStatusEnum.NEW.name());
            require(memberCoupons.save(issued), "优惠券发放失败"); result.setMemberCouponId(issued.getId());
        }
        if (!"THANKS".equals(selected.getType())) {
            selected.setIssued(selected.getIssued() + 1); require(prizes.updateById(selected) == 1, "奖品库存更新失败");
        }
        require(records.insert(result) == 1, "抽奖记录保存失败"); return result;
    }

    /**
     * 按会员及活动筛选流水，买家会员ID始终来自登录上下文。
     */
    @Override
    public IPage<LotteryRecord> records(PageVO page, String memberId, String activityId) {
        return records.selectPage(page(page), new LambdaQueryWrapper<LotteryRecord>().eq(memberId != null, LotteryRecord::getMemberId, memberId)
            .eq(activityId != null && !activityId.isBlank(), LotteryRecord::getActivityId, activityId)
            .orderByDesc(LotteryRecord::getCreateTime).orderByDesc(LotteryRecord::getId));
    }

    /**
     * 固定万分概率选择，不按剩余库存重新归一化。
     *
     * @param pool 有序奖池
     * @param ticket 0至9999随机数
     * @return 命中奖项
     */
    static LotteryPrize select(List<LotteryPrize> pool, int ticket) {
        require(ticket >= 0 && ticket < 10000, "随机数超出范围");
        int cursor = 0;
        for (LotteryPrize prize : pool) { cursor += prize.getWeight(); if (ticket < cursor) return prize; }
        throw error("奖池概率配置不完整");
    }

    /**
     * 取统一时钟的当前时刻，资格与次数使用同一业务时刻。
     */
    private Date now() { return Date.from(clock.instant()); }

    /**
     * 构建受控分页，忽略外部动态排序。
     *
     * @param page 分页参数
     * @return 安全分页
     */
    private <T> Page<T> page(PageVO page) {
        return new Page<>(Math.max(1, Objects.requireNonNullElse(page.getPageNumber(), 1)),
                Math.max(1, Math.min(100, Objects.requireNonNullElse(page.getPageSize(), 10))));
    }

    /**
     * 锁定并校验活动存在。
     *
     * @param id 活动ID
     * @return 当前活动
     */
    private LotteryActivity activity(String id) {
        LotteryActivity row = activities.lock(id); require(row != null, "抽奖活动不存在"); return row;
    }

    /**
     * 按北京时间统计当天使用次数，未中奖同样计数。
     */
    private long used(String id, String member, LocalDate day) {
        return records.selectCount(new LambdaQueryWrapper<LotteryRecord>().eq(LotteryRecord::getActivityId, id)
                .eq(LotteryRecord::getMemberId, member).eq(LotteryRecord::getDrawDay, day));
    }

    /**
     * 复用积分服务，流水描述记录本次活动、奖项及积分的业务快照；请求号由抽奖记录负责防重。
     * 持有会员行锁期间核对流水数量恰好增加一条，避免已有奖励流水掩盖本次落库失败；失败由外层事务回滚。
     */
    private void awardPoints(Member member, LotteryRecord result) {
        long points = result.getPoints();
        require(member.getPoint() != null && member.getTotalPoint() != null
            && member.getPoint() <= 9007199254740991L - points && member.getTotalPoint() <= 9007199254740991L - points, "会员积分超出支持范围");
        String content = "抽奖奖励：参与「" + result.getActivityName() + "」，获得「" + result.getPrizeName() + "」，奖励" + points + "积分";
        var historyQuery = new LambdaQueryWrapper<MemberPointsHistory>().eq(MemberPointsHistory::getMemberId, member.getId())
            .eq(MemberPointsHistory::getPointSource, "LOTTERY").eq(MemberPointsHistory::getContent, content);
        long beforeCount = histories.selectCount(historyQuery);
        require(Boolean.TRUE.equals(memberService.updateMemberPoint(points, PointTypeEnum.INCREASE.name(), member.getId(), content, "LOTTERY")), "积分发放失败");
        require(histories.selectCount(historyQuery) == beforeCount + 1, "积分流水保存失败");
    }

    /**
     * 校验活动券归属、有效期和发行库存；领取时禁止固定券尚未生效。
     */
    private boolean validCoupon(Coupon coupon, Date now, boolean issuing) {
        if (coupon == null || Boolean.TRUE.equals(coupon.getDeleteFlag()) || !"0".equals(coupon.getStoreId()) || !"ACTIVITY".equals(coupon.getGetType())) return false;
        boolean valid = "DYNAMICTIME".equals(coupon.getRangeDayType()) && coupon.getEffectiveDays() != null && coupon.getEffectiveDays() >= 1 && coupon.getEffectiveDays() <= 365
            || "FIXEDTIME".equals(coupon.getRangeDayType()) && coupon.getStartTime() != null && coupon.getEndTime() != null && coupon.getEndTime().after(now) && (!issuing || !coupon.getStartTime().after(now));
        return valid && (!issuing || coupon.getPublishNum() != null && coupon.getReceivedNum() != null && (coupon.getPublishNum() == 0 || coupon.getReceivedNum() < coupon.getPublishNum()));
    }

    /**
     * 校验新建、编辑及首次发布的百分比为正整数；存储仍使用万分点，不能通过接口绕过表单。
     */
    private static boolean validPercentage(Integer weight) { return weight != null && weight >= 100 && weight <= 10000 && weight % 100 == 0; }

    /**
     * 校验必填文本长度。
     */
    private static boolean text(String value, int max) { return value != null && !value.isBlank() && value.length() <= max; }
    /**
     * 不满足业务约束时终止并回滚。
     */
    private static void require(boolean valid, String message) { if (!valid) throw error(message); }
    /**
     * 创建可展示的参数业务异常。
     */
    private static ServiceException error(String message) { return new ServiceException(ResultCode.PARAMS_ERROR, message); }
}
