package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dos.DistributionGoods;
import cn.lili.modules.distribution.entity.dos.DistributionGrade;
import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.dos.DistributionWalletLog;
import cn.lili.modules.distribution.entity.enums.DistributionGoodsAuthEnum;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.entity.enums.DistributionWalletLogTypeEnum;
import cn.lili.modules.distribution.mapper.DistributionWalletLogMapper;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionOrderService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.service.DistributionWalletAccountService;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.enums.PayStatusEnum;
import cn.lili.modules.order.order.service.OrderItemService;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.AbstractWrapper;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 有佣金分销单入账：冻结余额、流水类型、佣金公式与双路径幂等。
 *
 * @author Mike
 * @date 2026-09-07
 */
class DistributionRewardServiceImplPaymentRewardsTest {

    private static final String ORDER_SN = "O001";
    private static final String ORDER_ITEM_SN = "OI1";
    private static final String DIRECT_ID = "D1";
    private static final String INVITE_ID = "D2";
    private static final String BUYER_DIST_ID = "B1";
    private static final Date PAYMENT_TIME = new Date(1_700_000_000_000L);
    private static final Date CREATE_TIME = new Date(1_699_000_000_000L);

    private DistributionRewardServiceImpl rewardService;
    private OrderService orderService;
    private OrderItemService orderItemService;
    private DistributionOrderService distributionOrderService;
    private DistributionGoodsService distributionGoodsService;
    private DistributionService distributionService;
    private DistributionGradeService distributionGradeService;
    private DistributionCustomerRelationService customerRelationService;
    private DistributionWalletLogMapper walletLogMapper;
    private SettingService settingService;

    private final List<DistributionOrder> storedOrders = new ArrayList<>();
    private final List<DistributionWalletLog> walletLogs = new ArrayList<>();
    private Distribution direct;
    private Distribution invite;

    @BeforeAll
    static void initMybatisEntityCache() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, Distribution.class);
        TableInfoHelper.initTableInfo(assistant, DistributionOrder.class);
        TableInfoHelper.initTableInfo(assistant, DistributionWalletLog.class);
        TableInfoHelper.initTableInfo(assistant, DistributionGoods.class);
        TableInfoHelper.initTableInfo(assistant, Order.class);
        TableInfoHelper.initTableInfo(assistant, OrderItem.class);
    }

    @BeforeEach
    void setUp() {
        storedOrders.clear();
        walletLogs.clear();

        rewardService = new DistributionRewardServiceImpl();
        orderService = mock(OrderService.class);
        orderItemService = mock(OrderItemService.class);
        distributionOrderService = mock(DistributionOrderService.class);
        distributionGoodsService = mock(DistributionGoodsService.class);
        distributionService = mock(DistributionService.class);
        distributionGradeService = mock(DistributionGradeService.class);
        customerRelationService = mock(DistributionCustomerRelationService.class);
        walletLogMapper = mock(DistributionWalletLogMapper.class);
        settingService = mock(SettingService.class);

        DistributionWalletAccountService walletAccountService = new DistributionWalletAccountServiceImpl();
        ReflectionTestUtils.setField(walletAccountService, "distributionService", distributionService);
        ReflectionTestUtils.setField(walletAccountService, "distributionWalletLogMapper", walletLogMapper);

        ReflectionTestUtils.setField(rewardService, "orderService", orderService);
        ReflectionTestUtils.setField(rewardService, "orderItemService", orderItemService);
        ReflectionTestUtils.setField(rewardService, "distributionOrderService", distributionOrderService);
        ReflectionTestUtils.setField(rewardService, "distributionGoodsService", distributionGoodsService);
        ReflectionTestUtils.setField(rewardService, "distributionService", distributionService);
        ReflectionTestUtils.setField(rewardService, "distributionGradeService", distributionGradeService);
        ReflectionTestUtils.setField(rewardService, "customerRelationService", customerRelationService);
        ReflectionTestUtils.setField(rewardService, "walletAccountService", walletAccountService);
        ReflectionTestUtils.setField(rewardService, "settingService", settingService);

        when(settingService.get(anyString())).thenReturn(null);
        when(distributionOrderService.list(org.mockito.ArgumentMatchers.<Wrapper<DistributionOrder>>any()))
                .thenAnswer(invocation -> new ArrayList<>(storedOrders));
        when(distributionOrderService.save(any(DistributionOrder.class))).thenAnswer(invocation -> {
            storedOrders.add(invocation.getArgument(0));
            return true;
        });
        when(distributionOrderService.getOne(org.mockito.ArgumentMatchers.<Wrapper<DistributionOrder>>any(), anyBoolean()))
                .thenAnswer(invocation -> storedOrders.stream().findFirst().orElse(null));
        when(walletLogMapper.selectCount(any())).thenAnswer(invocation -> countLogs(invocation.getArgument(0)));
        when(walletLogMapper.insert(any(DistributionWalletLog.class))).thenAnswer(invocation -> {
            walletLogs.add(invocation.getArgument(0));
            return 1;
        });
        when(distributionService.updateById(any(Distribution.class))).thenReturn(true);
    }

    /**
     * 入账冻结佣金并写冻结流水。
     */
    @Test
    void handlePaymentRewardsFreezesCommissionAndWritesFreezeLog() {
        stubPaidDistributionOrder();

        // 佣金池 20 × 0.10 / 0.20 → 直推冻结 2、邀请冻结 4
        rewardService.handlePaymentRewards(ORDER_SN);

        assertThat(storedOrders).hasSize(1);
        DistributionOrder stored = storedOrders.get(0);
        assertThat(stored.getDirectCommission()).isEqualTo(2.0D);
        assertThat(stored.getInviteCommission()).isEqualTo(4.0D);
        assertThat(stored.getSelfPurchase()).isFalse();
        assertThat(stored.getPerformanceDistributorId()).isEqualTo(DIRECT_ID);
        assertThat(stored.getPaymentTime()).isEqualTo(PAYMENT_TIME);
        assertThat(stored.getCommissionPool()).isEqualByComparingTo("20.00");
        assertThat(BigDecimal.valueOf(direct.getCommissionFrozen())).isEqualByComparingTo("2.00");
        assertThat(BigDecimal.valueOf(invite.getCommissionFrozen())).isEqualByComparingTo("4.00");

        assertThat(walletLogs).hasSize(2);
        assertThat(walletLogs).allMatch(log ->
                DistributionWalletLogTypeEnum.COMMISSION_FREEZE.name().equals(log.getBizType()));
        assertThat(walletLogs).extracting(DistributionWalletLog::getChangeAmount)
                .usingElementComparator(BigDecimal::compareTo)
                .containsExactlyInAnyOrder(new BigDecimal("2.00"), new BigDecimal("4.00"));
        assertThat(walletLogs).extracting(DistributionWalletLog::getAfterCommissionFrozen)
                .usingElementComparator(BigDecimal::compareTo)
                .containsExactlyInAnyOrder(new BigDecimal("2.00"), new BigDecimal("4.00"));

        verify(distributionGradeService).refreshGrade(DIRECT_ID);
    }

    /**
     * 分销员自购且模式为 SELF：行上自购为 true，受益人是买家本人。
     */
    @Test
    void handlePaymentRewardsMarksSelfPurchaseWhenBuyerIsDirectBeneficiary() {
        stubPaidDistributionOrder();
        when(distributionService.getOne(org.mockito.ArgumentMatchers.<Wrapper<Distribution>>any())).thenReturn(direct);

        rewardService.handlePaymentRewards(ORDER_SN);

        assertThat(storedOrders).hasSize(1);
        DistributionOrder stored = storedOrders.get(0);
        assertThat(stored.getDistributionId()).isEqualTo(DIRECT_ID);
        assertThat(stored.getPerformanceDistributorId()).isEqualTo(DIRECT_ID);
        assertThat(stored.getSelfPurchase()).isTrue();
        assertThat(stored.getDirectCommission()).isEqualTo(2.0D);
        assertThat(stored.getInviteCommission()).isEqualTo(4.0D);
    }

    /**
     * 分销员自购且模式为 BOUND_DISTRIBUTOR：佣金归绑定方，行上自购为 false。
     */
    @Test
    void handlePaymentRewardsKeepsBoundOrderAsPromotionNotSelfPurchase() {
        stubPaidDistributionOrder();
        Distribution buyer = distributor(BUYER_DIST_ID, "买家分销员", null);
        when(distributionService.getOne(org.mockito.ArgumentMatchers.<Wrapper<Distribution>>any())).thenReturn(buyer);
        Setting setting = new Setting();
        setting.setSettingValue("{\"selfPurchaseCommissionMode\":\"BOUND_DISTRIBUTOR\"}");
        when(settingService.get(anyString())).thenReturn(setting);

        rewardService.handlePaymentRewards(ORDER_SN);

        assertThat(storedOrders).hasSize(1);
        DistributionOrder stored = storedOrders.get(0);
        assertThat(stored.getDistributionId()).isEqualTo(DIRECT_ID);
        assertThat(stored.getSelfPurchase()).isFalse();
        assertThat(stored.getDirectCommission()).isEqualTo(2.0D);
        assertThat(stored.getInviteCommission()).isEqualTo(4.0D);
    }

    /**
     * 双路径重复入账幂等，不重复写单、不重复加冻结。
     */
    @Test
    void handlePaymentRewardsIsIdempotentAcrossDualPath() {
        stubPaidDistributionOrder();

        // 模拟 afterCommit 与 MQ 补偿各入一次
        rewardService.handlePaymentRewards(ORDER_SN);
        rewardService.handlePaymentRewards(ORDER_SN);

        verify(distributionOrderService, times(1)).save(any(DistributionOrder.class));
        assertThat(storedOrders).hasSize(1);
        assertThat(walletLogs).hasSize(2);
        assertThat(BigDecimal.valueOf(direct.getCommissionFrozen())).isEqualByComparingTo("2.00");
        assertThat(BigDecimal.valueOf(invite.getCommissionFrozen())).isEqualByComparingTo("4.00");
    }

    /**
     * 非分销商品不入账。
     */
    @Test
    void handlePaymentRewardsSkipsNonDistributionGoods() {
        stubPaidOrderWithoutDistributionGoods();

        // 无分销商品，不应写单、不应冻结
        rewardService.handlePaymentRewards(ORDER_SN);

        verify(distributionOrderService, never()).save(any(DistributionOrder.class));
        verify(walletLogMapper, never()).insert(any(DistributionWalletLog.class));
        assertThat(direct.getCommissionFrozen()).isEqualTo(0D);
        verify(distributionGradeService, never()).refreshGrade(anyString());
    }

    /**
     * 未支付订单不入账。
     */
    @Test
    void handlePaymentRewardsSkipsUnpaidOrder() {
        Order order = baseOrder();
        order.setPayStatus(PayStatusEnum.UNPAID.name());
        when(orderService.getBySn(ORDER_SN)).thenReturn(order);

        // 未支付订单不入账
        rewardService.handlePaymentRewards(ORDER_SN);

        verify(customerRelationService, never()).bindPermanentOnPaid(anyString(), anyString());
        verify(distributionOrderService, never()).save(any(DistributionOrder.class));
        verify(walletLogMapper, never()).insert(any(DistributionWalletLog.class));
    }

    /**
     * 构造已支付且含分销商品的订单桩。
     */
    private void stubPaidDistributionOrder() {
        Order order = baseOrder();
        when(orderService.getBySn(ORDER_SN)).thenReturn(order);
        when(orderItemService.getByOrderSn(ORDER_SN)).thenReturn(List.of(paidItem()));

        DistributionGoods goods = new DistributionGoods();
        goods.setSkuId("SKU1");
        goods.setCommission(10D);
        goods.setAuthStatus(DistributionGoodsAuthEnum.PASS.name());
        when(distributionGoodsService.distributionGoods(anyList())).thenReturn(List.of(goods));

        DistributionCustomerRelation relation = new DistributionCustomerRelation();
        relation.setMemberId("M1");
        relation.setDistributionId(DIRECT_ID);
        when(customerRelationService.getActiveRelation("M1")).thenReturn(relation);
        when(distributionService.getOne(org.mockito.ArgumentMatchers.<Wrapper<Distribution>>any())).thenReturn(null);

        direct = distributor(DIRECT_ID, "直推", INVITE_ID);
        invite = distributor(INVITE_ID, "邀请", null);
        when(distributionService.getById(anyString())).thenAnswer(invocation -> {
            String id = invocation.getArgument(0);
            if (DIRECT_ID.equals(id)) {
                return direct;
            }
            if (INVITE_ID.equals(id)) {
                return invite;
            }
            return null;
        });

        DistributionGrade grade = new DistributionGrade();
        grade.setSortOrder(1);
        grade.setGoodsCommissionRate(new BigDecimal("0.10"));
        grade.setInviteCommissionRate(new BigDecimal("0.20"));
        when(distributionGradeService.resolveGradeWithFallback(any())).thenReturn(grade);
    }

    /**
     * 构造已支付但无分销商品的订单桩。
     */
    private void stubPaidOrderWithoutDistributionGoods() {
        stubPaidDistributionOrder();
        when(distributionGoodsService.distributionGoods(anyList())).thenReturn(List.of());
    }

    /**
     * 构造基础已支付订单。
     */
    private static Order baseOrder() {
        Order order = new Order();
        order.setSn(ORDER_SN);
        order.setPayStatus(PayStatusEnum.PAID.name());
        order.setMemberId("M1");
        order.setMemberName("buyer");
        order.setStoreId("S1");
        order.setStoreName("store");
        order.setPaymentTime(PAYMENT_TIME);
        order.setCreateTime(CREATE_TIME);
        return order;
    }

    /**
     * 构造已支付订单项。
     */
    private static OrderItem paidItem() {
        OrderItem item = new OrderItem();
        item.setSn(ORDER_ITEM_SN);
        item.setSkuId("SKU1");
        item.setGoodsId("G1");
        item.setGoodsName("goods");
        item.setNum(2);
        item.setFlowPrice(100D);
        return item;
    }

    /**
     * 构造测试用分销员。
     */
    private static Distribution distributor(String id, String name, String parentId) {
        Distribution distribution = new Distribution();
        distribution.setId(id);
        distribution.setMemberName(name);
        distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
        distribution.setGradeId("1");
        distribution.setParentDistributionId(parentId);
        distribution.setCommissionFrozen(0D);
        distribution.setRebateTotal(0D);
        distribution.setCanRebate(0D);
        distribution.setValidSalesAmount(BigDecimal.ZERO);
        return distribution;
    }

    /**
     * MyBatis-Plus 3.5 在生成 SQL 前不填充 paramNameValuePairs，需先取 sqlSegment。
     */
    private Long countLogs(Wrapper<DistributionWalletLog> wrapper) {
        String haystack = "";
        try {
            haystack += wrapper.getSqlSegment();
        } catch (Exception ignored) {
            haystack += String.valueOf(wrapper);
        }
        if (wrapper instanceof AbstractWrapper<?, ?, ?> abstractWrapper) {
            haystack += abstractWrapper.getParamNameValuePairs();
        }
        final String text = haystack;
        return walletLogs.stream().anyMatch(log ->
                log.getIdempotentKey() != null && text.contains(log.getIdempotentKey())) ? 1L : 0L;
    }
}
