package cn.lili.modules.distribution.serviceimpl;

import cn.lili.modules.distribution.entity.dos.DistributionOrder;
import cn.lili.modules.distribution.entity.vos.DistributionOrderSearchParams;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.metadata.OrderItem;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * 分销订单分页走 MP page，条件不带 do. 别名，无 sort 时默认按创建时间倒序。
 */
class DistributionOrderServiceImplPageTest {

    private DistributionOrderServiceImpl service;

    @BeforeEach
    void setUp() {
        service = spy(new DistributionOrderServiceImpl());
        doReturn(new Page<DistributionOrder>()).when(service).page(any(IPage.class), any(Wrapper.class));
    }

    @Test
    void getDistributionOrderPageUsesUnaliasedWrapperAndDefaultSort() {
        DistributionOrderSearchParams params = new DistributionOrderSearchParams();
        params.setOrderSn("O2097203651776069632");

        service.getDistributionOrderPage(params);

        ArgumentCaptor<Page<DistributionOrder>> pageCaptor = ArgumentCaptor.forClass(Page.class);
        ArgumentCaptor<Wrapper<DistributionOrder>> wrapperCaptor = ArgumentCaptor.forClass(Wrapper.class);
        verify(service).page(pageCaptor.capture(), wrapperCaptor.capture());

        assertThat(pageCaptor.getValue().orders())
                .anyMatch(item -> "create_time".equals(item.getColumn()) && !item.isAsc());
        String sqlSegment = wrapperCaptor.getValue().getSqlSegment();
        assertThat(sqlSegment).contains("order_sn");
        assertThat(sqlSegment).doesNotContain("do.");
    }

    @Test
    void getDistributionOrderPageKeepsRequestedSort() {
        DistributionOrderSearchParams params = new DistributionOrderSearchParams();
        params.setSort("member_name");
        params.setOrder("asc");

        service.getDistributionOrderPage(params);

        ArgumentCaptor<Page<DistributionOrder>> pageCaptor = ArgumentCaptor.forClass(Page.class);
        verify(service).page(pageCaptor.capture(), any(Wrapper.class));
        assertThat(pageCaptor.getValue().orders())
                .extracting(OrderItem::getColumn)
                .containsExactly("member_name");
        assertThat(pageCaptor.getValue().orders().get(0).isAsc()).isTrue();
    }
}
