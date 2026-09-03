package cn.lili.controller.manager.goods;

import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import cn.lili.modules.goods.service.GoodsSearchParamService;
import cn.lili.modules.goods.service.GoodsService;
import cn.lili.modules.goods.service.GoodsSkuService;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;

import java.lang.reflect.Method;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 运营端参数倒排重建接口契约。
 *
 * @author Mike
 * @date 2026-09-03
 */
@ExtendWith(MockitoExtension.class)
class GoodsManagerControllerSearchParamRebuildTest {

    @InjectMocks
    private GoodsManagerController controller;

    @Mock
    private GoodsService goodsService;

    @Mock
    private GoodsSkuService goodsSkuService;

    @Mock
    private GoodsSearchParamService goodsSearchParamService;

    @Test
    void submitRebuildReturnsServiceStatus() {
        RebuildStatus status = new RebuildStatus();
        status.setRunning(true);
        status.setMessage("已提交");
        when(goodsSearchParamService.submitRebuildAll()).thenReturn(status);

        ResultMessage<RebuildStatus> result = controller.submitSearchParamRebuild();

        assertThat(result.isSuccess()).isTrue();
        assertThat(result.getResult()).isSameAs(status);
        verify(goodsSearchParamService).submitRebuildAll();
    }

    @Test
    void rebuildByGoodsIdReturnsWrittenRows() {
        when(goodsSearchParamService.rebuildByGoodsId("g1")).thenReturn(3);

        ResultMessage<Integer> result = controller.rebuildSearchParamByGoodsId("g1");

        assertThat(result.getResult()).isEqualTo(3);
        verify(goodsSearchParamService).rebuildByGoodsId("g1");
    }

    @Test
    void getStatusReturnsRedisProgress() {
        RebuildStatus status = new RebuildStatus();
        status.setRunning(false);
        status.setMessage("完成");
        when(goodsSearchParamService.getRebuildStatus()).thenReturn(status);

        ResultMessage<RebuildStatus> result = controller.getSearchParamRebuildStatus();

        assertThat(result.getResult()).isSameAs(status);
        verify(goodsSearchParamService).getRebuildStatus();
    }

    @Test
    void rebuildEndpointsAreDemoSiteProtected() throws NoSuchMethodException {
        Method submit = GoodsManagerController.class.getMethod("submitSearchParamRebuild");
        Method one = GoodsManagerController.class.getMethod("rebuildSearchParamByGoodsId", String.class);
        Method status = GoodsManagerController.class.getMethod("getSearchParamRebuildStatus");

        assertThat(submit.getAnnotation(DemoSite.class)).isNotNull();
        assertThat(one.getAnnotation(DemoSite.class)).isNotNull();
        assertThat(status.getAnnotation(DemoSite.class)).isNotNull();

        assertThat(submit.getAnnotation(PostMapping.class).value()).containsExactly("/search-param/rebuild");
        assertThat(one.getAnnotation(PostMapping.class).value()).containsExactly("/search-param/rebuild/{goodsId}");
        assertThat(status.getAnnotation(GetMapping.class).value()).containsExactly("/search-param/rebuild/status");
    }
}
