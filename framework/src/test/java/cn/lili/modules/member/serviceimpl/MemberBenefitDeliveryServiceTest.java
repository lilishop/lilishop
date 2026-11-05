package cn.lili.modules.member.serviceimpl;

import cn.lili.modules.member.service.MemberBenefitDeliveryService;

import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.*;
import cn.lili.modules.member.mapper.MemberBenefitDeliveryMapper;
import cn.lili.modules.member.service.MemberService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.support.SimpleTransactionStatus;
import java.util.function.BiConsumer;
import static org.assertj.core.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * 普通等级权益账本快照、防重及失败事务调用测试；使用模拟存储，不验证真实数据库事务。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
class MemberBenefitDeliveryServiceTest {
    /** 模拟发放账本预占、行锁及结果写入。 */
    private final MemberBenefitDeliveryMapper mapper=mock(MemberBenefitDeliveryMapper.class);
    /** 模拟领取客户查询及行锁。 */
    private final MemberService members=mock(MemberService.class);
    /** 模拟事务API，仅验证提交和回滚调用，不代表真实数据库事务。 */
    private final PlatformTransactionManager transactions=mock(PlatformTransactionManager.class);
    /** 被测试的权益逐项发放服务。 */
    private final MemberBenefitDeliveryService service=new MemberBenefitDeliveryServiceImpl();
    /** 用于补发、防重和失败场景的账本快照。 */
    private final MemberBenefitDelivery record=new MemberBenefitDelivery();
    /**
     * 准备失败账本、客户及模拟事务管理器，用于验证发放回调与事务API调用。
     * 不连接真实数据库，不能据此认定多连接锁及数据库回滚已验收。
     */
    @BeforeEach void setup() {
        ReflectionTestUtils.setField(service,"baseMapper",mapper); ReflectionTestUtils.setField(service,"memberService",members);
        ReflectionTestUtils.setField(service,"transactionManager",transactions);
        when(transactions.getTransaction(any())).thenAnswer(call->new SimpleTransactionStatus());
        record.setId("delivery");record.setMemberId("member");record.setBenefitId("benefit");record.setBenefitType("GIFT_POINT");
        record.setBenefitConfig("{\"giftPoint\":100}");record.setStatus("FAILED");record.setAttempts(1);
        when(mapper.lock("delivery")).thenReturn(record);
        Member member=new Member();member.setId("member");when(members.getOne(any())).thenReturn(member);
    }
    /**
     * 验证补发沿用账本中的原始配置快照，成功后再次请求不重复执行发放回调。
     */
    @Test void retryUsesOriginalConfigAndSuccessfulRecordCannotIssueAgain() {
        @SuppressWarnings("unchecked") BiConsumer<Member,MemberBenefit> grant=mock(BiConsumer.class);
        service.retry(record,grant);service.retry(record,grant);
        var benefit=org.mockito.ArgumentCaptor.forClass(MemberBenefit.class);
        verify(grant,times(1)).accept(any(),benefit.capture());
        assertThat(benefit.getValue().getBenefitConfig()).isEqualTo("{\"giftPoint\":100}");
        assertThat(record.getStatus()).isEqualTo("SUCCESS");
    }
    /**
     * 模拟发券库存不足，验证发放事务调用回滚、另行保存失败原因且不写成功状态。
     */
    @Test void failedGrantRollsBackAndPersistsReasonSeparately() {
        assertThatThrownBy(()->service.retry(record,(member,benefit)->{throw new IllegalStateException("优惠券库存不足");})).isInstanceOf(ServiceException.class);
        verify(transactions).rollback(any());
        verify(mapper).failed("delivery","优惠券库存不足");
        verify(mapper,never()).updateById(any(MemberBenefitDelivery.class));
    }
    /**
     * 验证SUCCESS记录直接跳过，不执行回调，也不因重复请求改写为失败。
     */
    @Test void successNeverBecomesFailureOnRepeatedRetry() {
        record.setStatus("SUCCESS");
        service.retry(record,(member,benefit)->{throw new AssertionError("不应重复发放");});
        verify(mapper,never()).failed(anyString(),anyString());
    }
}
