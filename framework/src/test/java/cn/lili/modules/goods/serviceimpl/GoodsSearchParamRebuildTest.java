package cn.lili.modules.goods.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSearchParam;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.service.GoodsSearchParamService;
import com.baomidou.mybatisplus.core.MybatisConfiguration;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.metadata.TableInfoHelper;
import org.apache.ibatis.builder.MapperBuilderAssistant;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static cn.lili.modules.goods.serviceimpl.GoodsSearchParamServiceImpl.REBUILD_BATCH_SIZE;
import static cn.lili.modules.goods.serviceimpl.GoodsSearchParamServiceImpl.REBUILD_LOCK_KEY;
import static cn.lili.modules.goods.serviceimpl.GoodsSearchParamServiceImpl.REBUILD_STATUS_KEY;
import static cn.lili.modules.goods.serviceimpl.GoodsSearchParamServiceImpl.REBUILD_STATUS_TTL_SECONDS;
import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyCollection;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.atLeastOnce;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

/**
 * 全量重建：controller 只提交；锁在 worker 内 tryLock；进度写入 Redis 并重置 TTL。
 *
 * @author Mike
 * @date 2026-09-03
 */
class GoodsSearchParamRebuildTest {

    private GoodsSearchParamServiceImpl service;
    private GoodsMapper goodsMapper;
    private RedissonClient redissonClient;
    private RLock lock;
    private Cache<RebuildStatus> cache;
    private ThreadPoolTaskExecutor rebuildExecutor;
    private GoodsSearchParamService self;

    @BeforeAll
    static void initTableInfo() {
        MapperBuilderAssistant assistant = new MapperBuilderAssistant(new MybatisConfiguration(), "");
        TableInfoHelper.initTableInfo(assistant, GoodsSearchParam.class);
        TableInfoHelper.initTableInfo(assistant, Goods.class);
    }

    @BeforeEach
    void setUp() {
        service = spy(new GoodsSearchParamServiceImpl());
        goodsMapper = mock(GoodsMapper.class);
        redissonClient = mock(RedissonClient.class);
        lock = mock(RLock.class);
        cache = mock(Cache.class);
        rebuildExecutor = mock(ThreadPoolTaskExecutor.class);
        self = mock(GoodsSearchParamService.class);

        ReflectionTestUtils.setField(service, "goodsMapper", goodsMapper);
        ReflectionTestUtils.setField(service, "redissonClient", redissonClient);
        ReflectionTestUtils.setField(service, "cache", cache);
        ReflectionTestUtils.setField(service, "rebuildExecutor", rebuildExecutor);
        ReflectionTestUtils.setField(service, "self", self);

        when(redissonClient.getLock(REBUILD_LOCK_KEY)).thenReturn(lock);
    }

    @Test
    void getRebuildStatusReturnsIdleWhenCacheEmpty() {
        when(cache.get(REBUILD_STATUS_KEY)).thenReturn(null);

        RebuildStatus status = service.getRebuildStatus();

        assertThat(status.getRunning()).isFalse();
        assertThat(status.getScanned()).isZero();
        assertThat(status.getRebuilt()).isZero();
        assertThat(status.getFailedGoodsIds()).isEmpty();
        assertThat(status.getMessage()).isEqualTo("未运行");
    }

    @Test
    void submitRebuildAllDoesNotEnqueueWhenAlreadyRunning() {
        RebuildStatus running = new RebuildStatus();
        running.setRunning(true);
        running.setScanned(12);
        running.setRebuilt(10);
        running.setMessage("运行中");
        when(cache.get(REBUILD_STATUS_KEY)).thenReturn(running);

        RebuildStatus result = service.submitRebuildAll();

        assertThat(result.getRunning()).isTrue();
        assertThat(result.getScanned()).isEqualTo(12);
        assertThat(result.getRebuilt()).isEqualTo(10);
        assertThat(result.getMessage()).isEqualTo("已有任务在运行");
        verify(rebuildExecutor, never()).execute(any());
        verify(cache, never()).put(eq(REBUILD_STATUS_KEY), any(), any());
    }

    @Test
    void submitRebuildAllEnqueuesWorkerAndLockIsTakenOnlyInsideWorker() throws InterruptedException {
        when(cache.get(REBUILD_STATUS_KEY)).thenReturn(null);
        ArgumentCaptor<Runnable> runnableCaptor = ArgumentCaptor.forClass(Runnable.class);

        RebuildStatus submitted = service.submitRebuildAll();

        assertThat(submitted.getRunning()).isTrue();
        assertThat(submitted.getMessage()).isEqualTo("已提交");
        verify(cache).put(eq(REBUILD_STATUS_KEY), any(RebuildStatus.class), eq(REBUILD_STATUS_TTL_SECONDS));
        verify(rebuildExecutor).execute(runnableCaptor.capture());
        verify(redissonClient, never()).getLock(anyString());
        verify(lock, never()).tryLock(eq(0L), eq(-1L), eq(TimeUnit.SECONDS));

        when(lock.tryLock(0, -1, TimeUnit.SECONDS)).thenReturn(true);
        when(lock.isHeldByCurrentThread()).thenReturn(true);
        when(goodsMapper.selectList(any())).thenReturn(List.of());

        runnableCaptor.getValue().run();

        verify(redissonClient).getLock(REBUILD_LOCK_KEY);
        verify(lock).tryLock(0, -1, TimeUnit.SECONDS);
        verify(lock).unlock();
    }

    @Test
    void workerExitsWithoutRebuildingWhenLockNotAcquired() throws InterruptedException {
        when(lock.tryLock(0, -1, TimeUnit.SECONDS)).thenReturn(false);
        RebuildStatus running = new RebuildStatus();
        running.setRunning(true);
        running.setScanned(3);
        running.setMessage("运行中");
        when(cache.get(REBUILD_STATUS_KEY)).thenReturn(running);

        ReflectionTestUtils.invokeMethod(service, "runRebuildAllWorker");

        verify(self, never()).rebuildByGoodsId(any());
        verify(lock, never()).unlock();
        ArgumentCaptor<RebuildStatus> statusCaptor = ArgumentCaptor.forClass(RebuildStatus.class);
        verify(cache).put(eq(REBUILD_STATUS_KEY), statusCaptor.capture(), eq(REBUILD_STATUS_TTL_SECONDS));
        assertThat(statusCaptor.getValue().getRunning()).isTrue();
        assertThat(statusCaptor.getValue().getScanned()).isEqualTo(3);
        assertThat(statusCaptor.getValue().getMessage()).isEqualTo("已有任务在运行");
    }

    @Test
    void workerRebuildsByIdCursorAndRecordsIllegalJsonAsFailed() throws InterruptedException {
        when(lock.tryLock(0, -1, TimeUnit.SECONDS)).thenReturn(true);
        when(lock.isHeldByCurrentThread()).thenReturn(true);
        when(self.rebuildByGoodsId("g1")).thenReturn(2);
        when(self.rebuildByGoodsId("g2")).thenThrow(new IllegalStateException("illegal goods params json, goodsId=g2"));

        AtomicInteger calls = new AtomicInteger();
        List<QueryWrapper<Goods>> wrappers = new ArrayList<>();
        when(goodsMapper.selectList(any())).thenAnswer(invocation -> {
            @SuppressWarnings("unchecked")
            QueryWrapper<Goods> wrapper = invocation.getArgument(0);
            wrappers.add(wrapper);
            int n = calls.getAndIncrement();
            if (n == 0) {
                return List.of(goods("g1"), goods("g2"));
            }
            return List.of();
        });

        ReflectionTestUtils.invokeMethod(service, "runRebuildAllWorker");

        assertThat(wrappers).hasSize(2);
        assertThat(wrappers.get(0).getSqlSelect()).isEqualTo("id");
        assertThat(wrappers.get(0).getSqlSegment()).contains("ORDER BY id ASC");
        assertThat(wrappers.get(0).getSqlSegment()).doesNotContain("params");
        assertThat(wrappers.get(0).getSqlSegment()).contains("LIMIT 500");
        assertThat(REBUILD_BATCH_SIZE).isEqualTo(500);
        assertThat(wrappers.get(1).getSqlSegment()).contains("id");

        verify(self).rebuildByGoodsId("g1");
        verify(self).rebuildByGoodsId("g2");
        verify(lock).unlock();

        ArgumentCaptor<RebuildStatus> statusCaptor = ArgumentCaptor.forClass(RebuildStatus.class);
        verify(cache, atLeastOnce()).put(eq(REBUILD_STATUS_KEY), statusCaptor.capture(), eq(REBUILD_STATUS_TTL_SECONDS));
        RebuildStatus finished = statusCaptor.getValue();
        assertThat(finished.getRunning()).isFalse();
        assertThat(finished.getScanned()).isEqualTo(2);
        assertThat(finished.getRebuilt()).isEqualTo(1);
        assertThat(finished.getFailedGoodsIds()).containsExactly("g2");
        assertThat(finished.getStartedAt()).isNotNull();
        assertThat(finished.getFinishedAt()).isNotNull();
        assertThat(finished.getMessage()).isEqualTo("完成");
    }

    @Test
    void rebuildByGoodsIdReadsCurrentParamsThenDeletesAndInserts() {
        Goods goods = new Goods();
        goods.setId("g1");
        goods.setParams("""
                [{"paramName":"内存","paramValue":"8G","isIndex":1,"sort":2}]
                """);
        when(goodsMapper.selectById("g1")).thenReturn(goods);
        doReturn(true).when(service).remove(any(Wrapper.class));
        doReturn(true).when(service).saveBatch(anyCollection());

        int written = service.rebuildByGoodsId("g1");

        assertThat(written).isEqualTo(1);
        verify(goodsMapper).selectById("g1");
        verify(service).remove(any(Wrapper.class));
        @SuppressWarnings("unchecked")
        ArgumentCaptor<List<GoodsSearchParam>> captor = ArgumentCaptor.forClass(List.class);
        verify(service).saveBatch(captor.capture());
        assertThat(captor.getValue()).hasSize(1);
        assertThat(captor.getValue().get(0).getGoodsId()).isEqualTo("g1");
        assertThat(captor.getValue().get(0).getParamName()).isEqualTo("内存");
        assertThat(captor.getValue().get(0).getParamValue()).isEqualTo("8G");
    }

    @Test
    void rebuildByGoodsIdThrowsAfterClearingIllegalJson() {
        Goods goods = new Goods();
        goods.setId("g-bad");
        goods.setParams("{not-an-array}");
        when(goodsMapper.selectById("g-bad")).thenReturn(goods);
        doReturn(true).when(service).remove(any(Wrapper.class));

        assertThatThrownBy(() -> service.rebuildByGoodsId("g-bad"))
                .isInstanceOf(IllegalStateException.class)
                .hasMessageContaining("g-bad");
        verify(service).remove(any(Wrapper.class));
        verify(service, never()).saveBatch(anyCollection());
    }

    @Test
    void eachProgressWriteResetsTtl() throws InterruptedException {
        when(lock.tryLock(0, -1, TimeUnit.SECONDS)).thenReturn(true);
        when(lock.isHeldByCurrentThread()).thenReturn(true);
        when(self.rebuildByGoodsId("g1")).thenReturn(1);
        when(goodsMapper.selectList(any())).thenReturn(List.of(goods("g1")), List.of());

        ReflectionTestUtils.invokeMethod(service, "runRebuildAllWorker");

        verify(cache, atLeastOnce()).put(eq(REBUILD_STATUS_KEY), any(RebuildStatus.class), eq(REBUILD_STATUS_TTL_SECONDS));
        verify(cache, never()).put(eq(REBUILD_STATUS_KEY), any(RebuildStatus.class));
    }

    private static Goods goods(String id) {
        Goods goods = new Goods();
        goods.setId(id);
        return goods;
    }
}
