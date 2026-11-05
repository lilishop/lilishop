package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.hutool.crypto.digest.DigestUtil;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.SnowFlake;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.PaidMemberOrder;
import cn.lili.modules.member.event.PaidMemberActivatedEvent;
import cn.lili.modules.member.mapper.PaidMemberOrderMapper;
import cn.lili.modules.member.service.MemberBenefitQueryService;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.member.service.PaidMemberService;
import cn.lili.modules.member.util.MemberBenefitRules;
import cn.lili.modules.member.util.PaidMemberRules;
import cn.lili.modules.store.entity.dos.Store;
import cn.lili.modules.store.service.StoreService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import cn.lili.mybatis.util.PageUtil;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEventPublisher;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.util.*;

/**
 * 单一付费会员：配置、订单、有效资格、续费与平台折扣。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
@Service("paidMemberService")
public class PaidMemberServiceImpl implements PaidMemberService {
    @Autowired
    private SettingService settings;
    @Autowired
    private PaidMemberOrderMapper orders;
    @Autowired
    private MemberService members;
    @Autowired
    private StoreService stores;
    @Autowired
    private GoodsSkuService skus;
    @Autowired
    private MemberBenefitQueryService benefits;
    @Autowired
    private ApplicationEventPublisher events;

    /**
     * 读取 li_setting 的单一会员配置，未配置或空值时返回默认关闭的设置。
     *
     * @return 当前平台配置
     */
    @Override
    public PaidMemberSetting config() {
        Setting setting = settings.get(SettingEnum.PAID_MEMBER_SETTING.name());
        return setting == null || CharSequenceUtil.isBlank(setting.getSettingValue()) ? new PaidMemberSetting() : JSONUtil.toBean(setting.getSettingValue(), PaidMemberSetting.class);
    }

    /**
     * 生成配置 JSON 的 SHA-256 摘要，用于购买确认及订单快照追溯。
     *
     * @param config 完整会员配置
     * @return 配置版本摘要
     */
    @Override
    public String version(PaidMemberSetting config) {
        return DigestUtil.sha256Hex(JSONUtil.toJsonStr(config));
    }

    /**
     * 检查权益、商家和 SKU 存在性以及折扣商品商家归属，清理富文本后保存统一设置。
     * 推荐商品只校验存在性，不限制为参与优惠的商家；保存失败回滚。
     *
     * @param config 平台提交的完整配置
     * @throws ServiceException 关联数据、业务规则非法或保存失败
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void saveConfig(PaidMemberSetting config) {
        PaidMemberRules.validate(config);
        if (!config.getBenefitIds().isEmpty() && benefits.listByIdList(config.getBenefitIds(), false).size() != new HashSet<>(config.getBenefitIds()).size())
            throw error("关联权益不存在或类型不支持");
        if ("CUSTOM".equals(config.getStoreScope()) && stores.listByIds(config.getStoreIds()).size() != new HashSet<>(config.getStoreIds()).size())
            throw error("选择的商家不存在");
        Set<String> skuIds = new HashSet<>(config.getRecommendedSkuIds());
        config.getGoodsRules().forEach(rule -> skuIds.add(rule.getSkuId()));
        var selectedSkus = skuIds.isEmpty() ? List.<GoodsSku>of() : skus.listByIds(skuIds);
        if (selectedSkus.size() != skuIds.size()) throw error("选择的商品SKU不存在");
        Set<String> discountSkuIds = new HashSet<>();
        config.getGoodsRules().forEach(rule -> discountSkuIds.add(rule.getSkuId()));
        Set<String> allowedStores = new HashSet<>(config.getStoreIds());
        if ("SELF".equals(config.getStoreScope()) && !discountSkuIds.isEmpty()) {
            var selectedStoreIds = selectedSkus.stream().filter(sku -> discountSkuIds.contains(sku.getId())).map(GoodsSku::getStoreId).collect(java.util.stream.Collectors.toSet());
            allowedStores.clear();
            stores.listByIds(selectedStoreIds).stream().filter(store -> Boolean.TRUE.equals(store.getSelfOperated())).forEach(store -> allowedStores.add(store.getId()));
        }
        if (selectedSkus.stream().anyMatch(sku -> discountSkuIds.contains(sku.getId()) && !allowedStores.contains(sku.getStoreId())))
            throw error("折扣商品必须属于参与付费会员的商家，请调整商家范围或移除商品");
        config.setDescription(MemberBenefitRules.description(config.getDescription()));
        config.setAgreement(MemberBenefitRules.description(config.getAgreement()));
        PaidMemberRules.validate(config);
        Setting entity = settings.get(SettingEnum.PAID_MEMBER_SETTING.name());
        if (entity == null) {
            entity = new Setting();
            entity.setId(SettingEnum.PAID_MEMBER_SETTING.name());
        }
        entity.setSettingValue(JSONUtil.toJsonStr(config));
        if (!settings.saveUpdate(entity)) throw error("付费会员配置保存失败");
    }

    /**
     * 查询已支付且当前处于 [startTime, endTime) 区间内的会员订单。
     *
     * @param memberId 客户 ID，空值返回 null
     * @return 当前有效订单，无有效资格时返回 null
     */
    @Override
    public PaidMemberOrder active(String memberId) {
        if (memberId == null || memberId.isBlank()) return null;
        Date now = new Date();
        return orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getMemberId, memberId)
                .eq(PaidMemberOrder::getStatus, "PAID").le(PaidMemberOrder::getStartTime, now).gt(PaidMemberOrder::getEndTime, now)
                .orderByDesc(PaidMemberOrder::getStartTime).last("limit 1"));
    }

    /**
     * 解析订单购买时的完整配置快照，后续平台修改不追溯改变已购折扣规则。
     *
     * @param order 会员购买订单
     * @return 成交配置快照
     */
    @Override
    public PaidMemberSetting snapshot(PaidMemberOrder order) {
        return JSONUtil.toBean(order.getConfigSnapshot(), PaidMemberSetting.class);
    }

    /**
     * 读取当前有效订单关联且仍处于开启状态的客户权益。
     *
     * @param memberId 客户 ID
     * @return 可用权益，无有效会员时为空列表
     */
    @Override
    public List<MemberBenefit> activeBenefits(String memberId) {
        var order = active(memberId);
        return order == null ? List.of() : benefits.listByIdList(snapshot(order).getBenefitIds(), true);
    }

    /**
     * 汇总会员中心配置、资格、到期时间、权益及推荐商品。
     * 节省金额按客户历史商品支付减退款的会员补贴累计统计，不受当前会员是否到期影响，推荐商品仅返回已上架且审核通过商品的买家展示字段。
     *
     * @param memberId 客户 ID
     * @return 展示数据，状态为 ACTIVE、UNOPENED 或 EXPIRED
     */
    @Override
    public Map<String, Object> current(String memberId) {
        PaidMemberSetting config = config();
        PaidMemberOrder active = active(memberId);
        PaidMemberOrder latest = orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getMemberId, memberId)
                .eq(PaidMemberOrder::getStatus, "PAID").orderByDesc(PaidMemberOrder::getEndTime).last("limit 1"));
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("config", config);
        result.put("configVersion", version(config));
        result.put("active", active != null);
        result.put("status", active != null ? "ACTIVE" : latest == null ? "UNOPENED" : "EXPIRED");
        result.put("endTime", latest == null ? null : latest.getEndTime());
        result.put("currentOrder", active);
        result.put("benefits", activeBenefits(memberId));
        result.put("offeredBenefits", config.getBenefitIds().isEmpty() ? List.of() : benefits.listByIdList(config.getBenefitIds(), true));
        result.put("savedAmount", orders.savedByMember(memberId));
        result.put("recommendedGoods", config.getRecommendedSkuIds().isEmpty() ? List.of() : skus.listByIds(config.getRecommendedSkuIds()).stream()
                .filter(s -> "UPPER".equals(s.getMarketEnable()) && "PASS".equals(s.getAuthFlag())).map(s -> {
                    // 买家响应仅包含展示字段，禁止直接返回含成本价的SKU实体。
                    Map<String, Object> item = new LinkedHashMap<>();
                    item.put("id", s.getId());
                    item.put("goodsId", s.getGoodsId());
                    item.put("goodsName", s.getGoodsName());
                    item.put("thumbnail", s.getThumbnail());
                    item.put("price", s.getPrice());
                    return item;
                }).toList());
        return result;
    }

    /**
     * 在客户行锁内按客户及请求编号复用订单，新建前校验协议与配置版本。
     * 新订单保存价格、自然月期限及配置快照，支付期限为创建后 30 分钟。
     *
     * @param memberId      购买客户 ID
     * @param requestId     8～64 位字母、数字、下划线或连字符组成的幂等编号
     * @param configVersion 客户确认购买时的配置版本
     * @param agreed        是否明确同意会员协议
     * @return 已存在或新建的购买记录，重复请求沿用原记录状态
     * @throws ServiceException 客户不存在、未同意协议、配置变更或暂停购买
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public PaidMemberOrder create(String memberId, String requestId, String configVersion, Boolean agreed) {
        if (!Boolean.TRUE.equals(agreed)) throw error("请先阅读并同意会员协议");
        if (requestId == null || !requestId.matches("[a-zA-Z0-9_-]{8,64}")) throw error("购买请求编号不正确");
        Member member = members.getOne(new LambdaQueryWrapper<Member>().eq(Member::getId, memberId).last("FOR UPDATE"));
        if (member == null) throw new ServiceException(ResultCode.USER_NOT_EXIST);
        PaidMemberOrder prior = orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getMemberId, memberId).eq(PaidMemberOrder::getRequestId, requestId));
        if (prior != null) return prior;
        PaidMemberSetting config = config();
        if (!Boolean.TRUE.equals(config.getEnabled())) throw error("付费会员暂未开放购买或续费");
        PaidMemberRules.validate(config);
        if (!version(config).equals(configVersion)) throw error("会员配置已更新，请刷新后重新确认购买");
        PaidMemberOrder order = new PaidMemberOrder();
        order.setId(SnowFlake.getIdStr());
        order.setSn("PM" + order.getId());
        order.setMemberId(memberId);
        order.setMemberName(member.getNickName());
        order.setRequestId(requestId);
        order.setAmount(config.getPrice());
        order.setMonths(config.getMonths());
        order.setStatus("UNPAID");
        order.setConfigSnapshot(JSONUtil.toJsonStr(config));
        order.setConfigVersion(version(config));
        order.setPayDeadline(new Date(System.currentTimeMillis() + 30 * 60 * 1000L));
        orders.insert(order);
        return order;
    }

    /**
     * 按业务单号读取会员订单，此方法不校验客户归属。
     *
     * @param sn PM 前缀会员订单号
     * @return 会员订单
     * @throws ServiceException 订单不存在
     */
    @Override
    public PaidMemberOrder order(String sn) {
        PaidMemberOrder order = orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getSn, sn));
        if (order == null) throw error("会员订单不存在");
        return order;
    }

    /**
     * 读取并校验订单归属，防止客户访问他人购买记录。
     *
     * @param memberId 当前客户 ID
     * @param sn       会员订单号
     * @return 当前客户的订单
     * @throws ServiceException 订单不存在或归属不匹配
     */
    @Override
    public PaidMemberOrder ownedOrder(String memberId, String sn) {
        PaidMemberOrder order = order(sn);
        if (!order.getMemberId().equals(memberId)) throw error("会员订单不存在");
        return order;
    }

    /**
     * 锁定并取消当前客户的未支付订单，已支付订单不可取消。
     *
     * @param memberId 当前客户 ID
     * @param sn       会员订单号
     * @throws ServiceException 订单不存在、归属不符或不是 UNPAID 状态
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void cancel(String memberId, String sn) {
        PaidMemberOrder order = orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getSn, sn).eq(PaidMemberOrder::getMemberId, memberId).last("FOR UPDATE"));
        if (order == null || !"UNPAID".equals(order.getStatus())) throw error("仅未支付订单可以取消");
        order.setStatus("CANCELLED");
        orders.updateById(order);
    }

    /**
     * 处理已经渠道核验的真实到账，依次锁定订单及客户，防止重复开通和并发续费重叠。
     * 核对金额及渠道后，已支付订单直接返回；续费从尚未到期的末次结束时间起算。
     * 取消或超时与真实到账竞态时仍承认到账一次；权益在事务提交后发放，失败交由周期任务重试。
     *
     * @param sn          会员订单号
     * @param amount      到账金额，单位元
     * @param method      支付渠道：WECHAT、ALIPAY、WALLET
     * @param transaction 渠道交易流水号
     * @throws ServiceException 金额或渠道不匹配、客户不存在
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void paid(String sn, Double amount, String method, String transaction) {
        if (method == null || !List.of("WECHAT", "ALIPAY", "WALLET").contains(method))
            throw error("不支持的会员支付方式");
        PaidMemberOrder order = orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getSn, sn).last("FOR UPDATE"));
        if (order == null || amount == null || !Double.isFinite(amount) || order.getAmount().compareTo(BigDecimal.valueOf(amount)) != 0)
            throw error("会员支付金额与订单不一致");
        if ("PAID".equals(order.getStatus())) return;
        Member member = members.getOne(new LambdaQueryWrapper<Member>().eq(Member::getId, order.getMemberId()).last("FOR UPDATE"));
        if (member == null) throw new ServiceException(ResultCode.USER_NOT_EXIST);
        Date now = new Date();
        PaidMemberOrder last = orders.selectOne(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getMemberId, member.getId()).eq(PaidMemberOrder::getStatus, "PAID")
                .orderByDesc(PaidMemberOrder::getEndTime).last("limit 1"));
        Date start = last != null && last.getEndTime().after(now) ? last.getEndTime() : now;
        order.setStatus("PAID");
        order.setPayTime(now);
        order.setStartTime(start);
        order.setEndTime(PaidMemberRules.endTime(start, order.getMonths()));
        order.setPaymentMethod(method);
        order.setReceivableNo(transaction);
        orders.updateById(order);
        events.publishEvent(new PaidMemberActivatedEvent(this, order.getId()));
        // 有效区间是资格的唯一依据；即使取消/超时与真实到账回调竞态，也承认有效到账一次。
    }

    /**
     * 分页查询会员购买记录；累计节省金额由会员中心按客户统计，不分摊到开通或续费订单。
     *
     * @param memberId 客户 ID，空值时不限制客户，由调用方控制查询权限
     * @param keyword  客户名称模糊查询条件
     * @param status   状态筛选，空值时不筛选
     * @param page     分页参数
     * @return 按创建时间及 ID 倒序排列的购买记录
     */
    @Override
    public IPage<PaidMemberOrder> page(String memberId, String keyword, String status, PageVO page) {
        IPage<PaidMemberOrder> result = orders.selectPage(PageUtil.initPage(page), new LambdaQueryWrapper<PaidMemberOrder>()
                .eq(CharSequenceUtil.isNotBlank(memberId), PaidMemberOrder::getMemberId, memberId)
                .like(CharSequenceUtil.isNotBlank(keyword), PaidMemberOrder::getMemberName, keyword)
                .eq(CharSequenceUtil.isNotBlank(status), PaidMemberOrder::getStatus, status)
                .orderByDesc(PaidMemberOrder::getCreateTime).orderByDesc(PaidMemberOrder::getId));
        return result;
    }

    /**
     * 按配置检查实物商品商家范围，获取统一或逐 SKU 折扣。
     * 此处不校验会员资格、活动互斥及上下架状态，由调用方负责。
     *
     * @param config 当前配置或有效订单的成交快照
     * @param sku    待计算商品 SKU
     * @return 折扣，单位折；商家、类型或 SKU 不适用时返回 null
     */
    @Override
    public BigDecimal discount(PaidMemberSetting config, GoodsSku sku) {
        boolean storeAllowed = "CUSTOM".equals(config.getStoreScope()) ? config.getStoreIds().contains(sku.getStoreId())
                : Optional.ofNullable(stores.getById(sku.getStoreId())).map(Store::getSelfOperated).orElse(false);
        if (!storeAllowed || !"PHYSICAL_GOODS".equals(sku.getGoodsType())) return null;
        return "ALL".equals(config.getDiscountMode()) ? config.getDiscount()
                : config.getGoodsRules().stream().filter(rule -> sku.getId().equals(rule.getSkuId())).map(PaidMemberSetting.GoodsRule::getDiscount).findFirst().orElse(null);
    }

    /**
     * 批量生成会员价：有效会员使用成交快照，未开通客户使用当前配置预览。
     * 过滤不适用、无实际折扣、活动专用、下架或未审核商品；图片标签始终使用当前展示配置。
     *
     * @param memberId 客户 ID
     * @param skuIds   SKU 主键列表，最多 100 个
     * @return 以 SKU ID 为键的报价，含元单位价格、折扣、资格及标签
     * @throws ServiceException 列表为空值或超出上限
     */
    @Override
    public Map<String, Object> quotes(String memberId, List<String> skuIds) {
        if (skuIds == null || skuIds.size() > 100) throw error("一次最多查询100个SKU");
        PaidMemberOrder active = active(memberId);
        PaidMemberSetting displayConfig = config();
        PaidMemberSetting config = active == null ? displayConfig : snapshot(active);
        Map<String, Object> result = new LinkedHashMap<>();
        if (active == null && !Boolean.TRUE.equals(config.getEnabled())) return result;
        for (GoodsSku sku : skuIds.isEmpty() ? List.<GoodsSku>of() : skus.listByIds(skuIds)) {
            BigDecimal discount = discount(config, sku);
            if (discount == null || discount.compareTo(BigDecimal.TEN) >= 0 || Boolean.TRUE.equals(sku.getPromotionFlag()) || !"UPPER".equals(sku.getMarketEnable()) || !"PASS".equals(sku.getAuthFlag()))
                continue;
            result.put(sku.getId(), Map.of("price", PaidMemberRules.price(BigDecimal.valueOf(sku.getPrice()), discount), "discount", discount, "eligible", active != null,
                    "label", Objects.toString(displayConfig.getPriceLabel(), "")));
        }
        return result;
    }

    /**
     * 按主键游标扫描当前有效的已支付订单，供周期权益任务处理。
     *
     * @param cursor 上批最后一个 ID，空值从头开始
     * @return ID 升序的有效订单，每批最多 200 条
     */
    @Override
    public List<PaidMemberOrder> activeOrders(String cursor) {
        Date now = new Date();
        return orders.selectList(new LambdaQueryWrapper<PaidMemberOrder>().eq(PaidMemberOrder::getStatus, "PAID")
                .le(PaidMemberOrder::getStartTime, now).gt(PaidMemberOrder::getEndTime, now)
                .gt(CharSequenceUtil.isNotBlank(cursor), PaidMemberOrder::getId, cursor)
                .orderByAsc(PaidMemberOrder::getId).last("limit 200"));
    }

    /**
     * 按主键定位权益等内部业务的来源会员订单。
     *
     * @param id li_paid_member_order 主键
     * @return 订单，不存在时返回 null
     */
    @Override
    public PaidMemberOrder findById(String id) {
        return orders.selectById(id);
    }

    /**
     * 判断订单是否已支付且当前处于左闭右开的有效区间。
     *
     * @param order 待判断订单，可为 null
     * @return 是否拥有当前有效资格
     */
    @Override
    public boolean isActive(PaidMemberOrder order) {
        Date now = new Date();
        return order != null && "PAID".equals(order.getStatus()) && !order.getStartTime().after(now) && order.getEndTime().after(now);
    }

    /**
     * 检查权益来源订单的有效期、客户归属、快照关联及权益当前开启状态。
     *
     * @param memberId  领取客户 ID
     * @param orderId   来源会员订单主键
     * @param benefitId 待发放权益主键
     * @return 是否允许使用该来源订单发放此权益
     */
    @Override
    public boolean isBenefitEligible(String memberId, String orderId, String benefitId) {
        var order = orders.selectById(orderId);
        return isActive(order) && order.getMemberId().equals(memberId) && snapshot(order).getBenefitIds().contains(benefitId)
                && benefits.listByIdList(List.of(benefitId), true).size() == 1;
    }

    /**
     * 创建会员参数及业务规则校验异常。
     *
     * @param message 面向调用方的错误说明
     * @return 使用 PARAMS_ERROR 结果码的异常
     */
    private ServiceException error(String message) {
        return new ServiceException(ResultCode.PARAMS_ERROR, message);
    }
}
