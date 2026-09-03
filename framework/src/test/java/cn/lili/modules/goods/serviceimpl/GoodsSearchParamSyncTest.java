package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.GoodsSearchParam;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.InOrder;

import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.inOrder;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;

/**
 * syncByGoodsId：先 DELETE 再 INSERT；解析为空时只 DELETE。
 *
 * @author Mike
 * @date 2026-09-03
 */
class GoodsSearchParamSyncTest {

    @Test
    void syncByGoodsIdDeletesThenInsertsParsedIndexedRows() {
        GoodsSearchParamServiceImpl service = spy(new GoodsSearchParamServiceImpl());
        doReturn(true).when(service).remove(any(Wrapper.class));
        doReturn(true).when(service).saveBatch(anyCollection());

        String json = """
                [{"paramName":"内存","paramValue":"8G","isIndex":1,"sort":1},{"paramName":"产地","paramValue":"广东","isIndex":0}]
                """;
        service.syncByGoodsId("goods-1", json);

        InOrder inOrder = inOrder(service);
        inOrder.verify(service).remove(any(Wrapper.class));
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<GoodsSearchParam>> captor = ArgumentCaptor.forClass(List.class);
        inOrder.verify(service).saveBatch(captor.capture());
        assertThat(captor.getValue()).hasSize(1);
        GoodsSearchParam row = captor.getValue().get(0);
        assertThat(row.getGoodsId()).isEqualTo("goods-1");
        assertThat(row.getParamName()).isEqualTo("内存");
        assertThat(row.getParamValue()).isEqualTo("8G");
        assertThat(row.getParamSort()).isEqualTo(1);
    }

    @Test
    void syncByGoodsIdOnlyDeletesWhenParseResultIsEmpty() {
        GoodsSearchParamServiceImpl service = spy(new GoodsSearchParamServiceImpl());
        doReturn(true).when(service).remove(any(Wrapper.class));

        service.syncByGoodsId("goods-empty", "[]");

        verify(service).remove(any(Wrapper.class));
        verify(service, never()).saveBatch(anyCollection());
    }

    @Test
    void syncByGoodsIdOnlyDeletesWhenIndexedFilterDropsAll() {
        GoodsSearchParamServiceImpl service = spy(new GoodsSearchParamServiceImpl());
        doReturn(true).when(service).remove(any(Wrapper.class));

        String json = """
                [{"paramName":"产地","paramValue":"广东","isIndex":0}]
                """;
        service.syncByGoodsId("goods-no-index", json);

        verify(service).remove(any(Wrapper.class));
        verify(service, never()).saveBatch(anyCollection());
    }
}
