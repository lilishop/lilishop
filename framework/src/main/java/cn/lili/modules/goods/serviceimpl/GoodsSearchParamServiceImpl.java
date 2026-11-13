package cn.lili.modules.goods.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.cache.Cache;
import cn.lili.modules.goods.config.GoodsSearchParamRebuildConfig;
import cn.lili.modules.goods.entity.dos.Goods;
import cn.lili.modules.goods.entity.dos.GoodsSearchParam;
import cn.lili.modules.goods.entity.dto.GoodsParamsItemDTO;
import cn.lili.modules.goods.entity.dto.RebuildStatus;
import cn.lili.modules.goods.mapper.GoodsMapper;
import cn.lili.modules.goods.mapper.GoodsSearchParamMapper;
import cn.lili.modules.goods.service.GoodsSearchParamService;
import cn.lili.modules.goods.support.GoodsSearchParamParser;
import com.alibaba.fastjson2.JSON;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.redisson.api.RLock;
import org.redisson.api.RedissonClient;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.transaction.TransactionDefinition;
import org.springframework.transaction.support.TransactionTemplate;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.concurrent.TimeUnit;

/**
 * 商品参数倒排：写侧按 goods_id 物理删除后重建；全量重建走专用单线程执行器。
 * 单商品重建通过事务模板开启独立事务，避免为事务代理注入自身；普通同步沿用商品保存事务。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Slf4j
@Service
public class GoodsSearchParamServiceImpl extends ServiceImpl<GoodsSearchParamMapper, GoodsSearchParam>
        implements GoodsSearchParamService {

    static final String REBUILD_STATUS_KEY = "lili:goods:search-param:rebuild:status";
    static final String REBUILD_LOCK_KEY = "lili:goods:search-param:rebuild:lock";
    static final long REBUILD_STATUS_TTL_SECONDS = 600L;
    static final int REBUILD_BATCH_SIZE = 500;

    /** 同一 goods_id 的 DELETE+INSERT 串行，避免并发编辑与重建撞唯一键。 */
    private static final Object[] GOODS_SYNC_STRIPES = new Object[64];

    static {
        for (int i = 0; i < GOODS_SYNC_STRIPES.length; i++) {
            GOODS_SYNC_STRIPES[i] = new Object();
        }
    }

    @Autowired
    private GoodsMapper goodsMapper;
    @Autowired
    private RedissonClient redissonClient;
    @Autowired
    private Cache<RebuildStatus> cache;
    @Autowired
    @Qualifier(GoodsSearchParamRebuildConfig.EXECUTOR_BEAN_NAME)
    private ThreadPoolTaskExecutor rebuildExecutor;
    private final TransactionTemplate rebuildTransaction;

    /**
     * 配置单商品重建的独立事务，存在外层事务时先挂起，避免一个商品失败影响其他商品。
     *
     * @param transactionManager 商品倒排写入使用的事务管理器
     */
    public GoodsSearchParamServiceImpl(PlatformTransactionManager transactionManager) {
        this.rebuildTransaction = new TransactionTemplate(transactionManager);
        this.rebuildTransaction.setPropagationBehavior(TransactionDefinition.PROPAGATION_REQUIRES_NEW);
    }

    @Override
    public void syncByGoodsId(String goodsId, String paramsJson) {
        // 写侧挂载点调用：与商品保存同事务，失败则 JSON 与倒排一并回滚
        doSync(goodsId, paramsJson);
    }

    /**
     * 在独立事务中现读商品参数并重建倒排，读取、删除、插入整体提交或回滚。
     * 参数非法或写入失败时回滚并抛出异常，全量任务由调用方记录失败商品后继续处理。
     *
     * @param goodsId 商品主键
     * @return 写入倒排表的行数
     */
    @Override
    public int rebuildByGoodsId(String goodsId) {
        return rebuildTransaction.execute(status -> doRebuildByGoodsId(goodsId));
    }

    /**
     * 读取当前商品参数并替换倒排数据；非法JSON抛出异常，由外层事务回滚本次修改。
     *
     * @param goodsId 商品主键，不存在的商品按空参数处理
     * @return 写入倒排表的行数
     */
    private int doRebuildByGoodsId(String goodsId) {
        Goods goods = goodsMapper.selectById(goodsId);
        String paramsJson = goods == null || goods.getParams() == null ? "[]" : goods.getParams();
        if (isIllegalParamsJson(paramsJson)) {
            log.warn("illegal goods params json, goodsId={}", goodsId);
            doSync(goodsId, "[]");
            throw new IllegalStateException("illegal goods params json, goodsId=" + goodsId);
        }
        return doSync(goodsId, paramsJson);
    }

    /**
     * 提交全量重建：若已有任务在跑则直接返回 busy 状态；否则写入 running 并丢进专用线程池。
     */
    @Override
    public RebuildStatus submitRebuildAll() {
        RebuildStatus current = getRebuildStatus();
        if (Boolean.TRUE.equals(current.getRunning())) {
            return copyBusy(current, "已有任务在运行");
        }
        RebuildStatus submitted = new RebuildStatus();
        submitted.setRunning(true);
        submitted.setMessage("已提交");
        writeStatus(submitted);
        rebuildExecutor.execute(this::runRebuildAllWorker);
        return submitted;
    }

    @Override
    public RebuildStatus getRebuildStatus() {
        RebuildStatus status = cache.get(REBUILD_STATUS_KEY);
        if (status == null) {
            RebuildStatus idle = new RebuildStatus();
            idle.setRunning(false);
            idle.setScanned(0);
            idle.setRebuilt(0);
            idle.setFailedGoodsIds(new ArrayList<>());
            idle.setMessage("未运行");
            return idle;
        }
        return status;
    }

    /**
     * 按 goods_id 物理删除后按可索引参数重建；解析结果为空则只删不插。
     *
     * @return 写入行数（仅 DELETE 时为 0）
     */
    private int doSync(String goodsId, String paramsJson) {
        // 同 goodsId 串行，避免并发编辑与 rebuild 撞唯一键
        synchronized (goodsSyncLock(goodsId)) {
            this.remove(new LambdaQueryWrapper<GoodsSearchParam>().eq(GoodsSearchParam::getGoodsId, goodsId));
            // 只写入 isIndex=1 且 trim/去重/长度合法的项
            List<GoodsParamsItemDTO> items = GoodsSearchParamParser.parseIndexed(paramsJson);
            if (items.isEmpty()) {
                return 0;
            }
            List<GoodsSearchParam> rows = new ArrayList<>(items.size());
            for (GoodsParamsItemDTO item : items) {
                GoodsSearchParam row = new GoodsSearchParam();
                row.setGoodsId(goodsId);
                row.setParamName(item.getParamName());
                row.setParamValue(item.getParamValue());
                row.setParamSort(item.getSort());
                rows.add(row);
            }
            this.saveBatch(rows);
            return rows.size();
        }
    }

    private static Object goodsSyncLock(String goodsId) {
        int hash = goodsId == null ? 0 : goodsId.hashCode();
        return GOODS_SYNC_STRIPES[(hash & 0x7fffffff) % GOODS_SYNC_STRIPES.length];
    }

    /**
     * 全量重建 worker。锁必须在本线程获取和释放：Redisson {@code RLock} 线程绑定，
     * controller 线程 {@code tryLock} 再交给 worker {@code unlock} 会抛 {@code IllegalMonitorStateException}；
     * 若 controller 持锁不放、指望 lease 过期，看门狗只给持有线程续期，请求返回后锁会在重建中途释放。
     * {@code tryLock(0, -1, ...)}：不等待、leaseTime=-1 启用看门狗，持有期间自动续期。
     */
    private void runRebuildAllWorker() {
        RLock lock = redissonClient.getLock(REBUILD_LOCK_KEY);
        boolean locked = false;
        try {
            locked = lock.tryLock(0, -1, TimeUnit.SECONDS);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            RebuildStatus status = getRebuildStatus();
            status.setRunning(false);
            status.setFinishedAt(new Date());
            status.setMessage("重建任务被中断");
            writeStatus(status);
            return;
        }
        if (!locked) {
            RebuildStatus status = getRebuildStatus();
            status.setRunning(true);
            status.setMessage("已有任务在运行");
            writeStatus(status);
            return;
        }
        try {
            RebuildStatus status = new RebuildStatus();
            status.setRunning(true);
            status.setScanned(0);
            status.setRebuilt(0);
            status.setFailedGoodsIds(new ArrayList<>());
            status.setStartedAt(new Date());
            status.setMessage("运行中");
            writeStatus(status);

            String lastId = null;
            while (true) {
                List<String> ids = listGoodsIdsAfter(lastId, REBUILD_BATCH_SIZE);
                if (ids.isEmpty()) {
                    break;
                }
                for (String goodsId : ids) {
                    status.setScanned(nullToZero(status.getScanned()) + 1);
                    try {
                        rebuildByGoodsId(goodsId);
                        status.setRebuilt(nullToZero(status.getRebuilt()) + 1);
                    } catch (Exception e) {
                        log.warn("rebuild goods search param failed, goodsId={}", goodsId, e);
                        if (status.getFailedGoodsIds() == null) {
                            status.setFailedGoodsIds(new ArrayList<>());
                        }
                        status.getFailedGoodsIds().add(goodsId);
                    }
                    lastId = goodsId;
                    writeStatus(status);
                }
            }
            status.setRunning(false);
            status.setFinishedAt(new Date());
            status.setMessage("完成");
            writeStatus(status);
        } catch (Exception e) {
            log.error("rebuild all goods search param failed", e);
            RebuildStatus status = getRebuildStatus();
            status.setRunning(false);
            status.setFinishedAt(new Date());
            status.setMessage("失败: " + e.getMessage());
            writeStatus(status);
        } finally {
            if (lock.isHeldByCurrentThread()) {
                lock.unlock();
            }
        }
    }

    /** 按 id 升序游标取下一批商品主键，不预取 params（每个商品在 rebuildByGoodsId 内现读）。 */
    private List<String> listGoodsIdsAfter(String lastId, int limit) {
        QueryWrapper<Goods> wrapper = new QueryWrapper<>();
        wrapper.select("id");
        if (CharSequenceUtil.isNotEmpty(lastId)) {
            wrapper.gt("id", lastId);
        }
        wrapper.orderByAsc("id");
        wrapper.last("LIMIT " + limit);
        return goodsMapper.selectList(wrapper).stream().map(Goods::getId).toList();
    }

    private void writeStatus(RebuildStatus status) {
        cache.put(REBUILD_STATUS_KEY, status, REBUILD_STATUS_TTL_SECONDS);
    }

    private static RebuildStatus copyBusy(RebuildStatus current, String message) {
        RebuildStatus busy = new RebuildStatus();
        busy.setRunning(true);
        busy.setScanned(current.getScanned());
        busy.setRebuilt(current.getRebuilt());
        busy.setFailedGoodsIds(current.getFailedGoodsIds());
        busy.setStartedAt(current.getStartedAt());
        busy.setFinishedAt(current.getFinishedAt());
        busy.setMessage(message);
        return busy;
    }

    private static int nullToZero(Integer value) {
        return value == null ? 0 : value;
    }

    private boolean isIllegalParamsJson(String paramsJson) {
        if (CharSequenceUtil.isEmpty(paramsJson)) {
            return false;
        }
        try {
            JSON.parseArray(paramsJson);
            return false;
        } catch (Exception e) {
            return true;
        }
    }
}
