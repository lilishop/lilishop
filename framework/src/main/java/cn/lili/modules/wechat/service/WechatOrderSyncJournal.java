package cn.lili.modules.wechat.service;

import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.vo.OrderSimpleVO;
import cn.lili.modules.order.order.service.OrderService;
import cn.lili.modules.wechat.entity.dos.WechatOrderSync;
import cn.lili.modules.wechat.entity.dos.WechatOrderSyncLog;
import cn.lili.modules.wechat.entity.vo.WechatOrderVO;
import cn.lili.modules.wechat.mapper.WechatOrderSyncMapper;
import cn.lili.modules.wechat.mapper.WechatOrderSyncLogMapper;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 微信支付流水级同步状态和操作日志；短事务独立提交，外部接口不占用数据库事务。
 * 调用方必须先持有支付流水分布式锁；接口已成功而落库失败时保留处理中记录供人工核查。
 *
 * @author Bulbasaur
 * @since 2026-11-09
 */
@Service
public class WechatOrderSyncJournal {
    @Autowired
    private WechatOrderSyncMapper syncMapper;
    @Autowired
    private WechatOrderSyncLogMapper logMapper;
    @Autowired
    private OrderService orderService;

    /** 支付流水缺失时使用订单级占位键，防止不同异常订单共用空键。 */
    public static String transactionKey(Order order) {
        return order.getReceivableNo() == null || order.getReceivableNo().isBlank()
                ? "ORDER:" + order.getSn() : order.getReceivableNo();
    }

    /** 在调用方持有流水锁后创建或读取状态，不对历史订单猜测成功。 */
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public WechatOrderSync ensure(Order order) {
        String key = transactionKey(order);
        WechatOrderSync sync = syncMapper.selectOne(new LambdaQueryWrapper<WechatOrderSync>()
                .eq(WechatOrderSync::getTransactionId, key));
        if (sync == null) {
            sync = new WechatOrderSync();
            sync.setId(UUID.randomUUID().toString().replace("-", ""));
            sync.setTransactionId(key);
            sync.setStatus("PENDING");
            sync.setCreateTime(new Date());
            sync.setUpdateTime(new Date());
            syncMapper.insert(sync);
        }
        return sync;
    }

    /** 每次执行先落处理中日志；进程中断也有可审计记录，重复成功事件后续标记 SKIPPED。 */
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public WechatOrderSyncLog begin(WechatOrderSync sync, Order order, String operation, AuthUser operator) {
        WechatOrderSyncLog log = new WechatOrderSyncLog();
        log.setId(UUID.randomUUID().toString().replace("-", ""));
        log.setTransactionId(sync.getTransactionId());
        log.setOrderSn(order.getSn());
        log.setOperation(operation);
        log.setTriggerType(operator == null ? "AUTO" : "MANUAL");
        log.setOperatorId(operator == null ? "SYSTEM" : operator.getId());
        log.setOperatorName(operator == null ? "系统" : operator.getUsername());
        log.setStatus("PROCESSING");
        log.setCreateTime(new Date());
        logMapper.insert(log);
        if ("SHIPPING".equals(operation)) {
            syncMapper.update(null, new LambdaUpdateWrapper<WechatOrderSync>().eq(WechatOrderSync::getId, sync.getId())
                    .set(WechatOrderSync::getStatus, "PROCESSING").set(WechatOrderSync::getUpdateTime, new Date()));
        }
        return log;
    }

    /** 发出 HTTP 请求之前保存请求版本及固定上传时间；超时重试复用相同版本，摘要不含明文凭据。 */
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void saveRequest(WechatOrderSync sync) {
        syncMapper.update(null, new LambdaUpdateWrapper<WechatOrderSync>().eq(WechatOrderSync::getId, sync.getId())
                .set(WechatOrderSync::getAppId, sync.getAppId()).set(WechatOrderSync::getMchId, sync.getMchId())
                .set(WechatOrderSync::getRequestHash, sync.getRequestHash()).set(WechatOrderSync::getUploadTime, sync.getUploadTime()));
    }

    /** 原子提交一次操作的结果；收货通知日志独立于发货状态，不能将发货成功改写为收货失败。 */
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public void finish(WechatOrderSync sync, WechatOrderSyncLog log, String outcome, String code, String message) {
        Date now = new Date();
        log.setStatus(outcome);
        log.setErrorCode(code);
        log.setErrorMessage(message);
        log.setFinishTime(now);
        log.setDurationMs(now.getTime() - log.getCreateTime().getTime());
        logMapper.updateById(log);
        LambdaUpdateWrapper<WechatOrderSync> update = new LambdaUpdateWrapper<WechatOrderSync>()
                .eq(WechatOrderSync::getId, sync.getId()).set(WechatOrderSync::getUpdateTime, now);
        if ("SHIPPING".equals(log.getOperation())) {
            boolean success = "SUCCESS".equals(outcome) || "SKIPPED".equals(outcome);
            update.set(WechatOrderSync::getStatus, success ? "SUCCESS" : outcome)
                    .set(WechatOrderSync::getErrorCode, code).set(WechatOrderSync::getErrorMessage, message)
                    .set(WechatOrderSync::getLastAttemptTime, now);
            if (success) update.set(WechatOrderSync::getSuccessTime,
                    "SKIPPED".equals(outcome) && sync.getSuccessTime() != null ? sync.getSuccessTime() : now);
            if ("SUCCESS".equals(outcome)) update.set(WechatOrderSync::getSuccessHash, sync.getRequestHash());
        } else if ("SUCCESS".equals(outcome)) {
            update.set(WechatOrderSync::getReceiveStatus, "SUCCESS").set(WechatOrderSync::getReceiveTime, now);
        }
        syncMapper.update(null, update);
    }

    /**
     * 将通用查询结果转为专用视图并批量回填上报信息，保留页号、页大小及总数。
     * 缺少本地记录统一标为待核查，不修改原分页及通用订单记录。
     * @param page 通用订单查询分页
     * @return 独立的小程序订单视图分页
     */
    public IPage<WechatOrderVO> fill(IPage<OrderSimpleVO> page) {
        Page<WechatOrderVO> result = new Page<>(page.getCurrent(), page.getSize(), page.getTotal());
        result.setRecords(page.getRecords().stream().map(WechatOrderVO::new).toList());
        if (page.getRecords().isEmpty()) return result;
        List<Order> orders = orderService.list(new LambdaQueryWrapper<Order>()
                .in(Order::getSn, page.getRecords().stream().map(OrderSimpleVO::getSn).toList()));
        Map<String, String> keys = orders.stream().collect(Collectors.toMap(Order::getSn, WechatOrderSyncJournal::transactionKey));
        Map<String, WechatOrderSync> states = keys.isEmpty() ? Map.of() : syncMapper.selectList(new LambdaQueryWrapper<WechatOrderSync>()
                        .in(WechatOrderSync::getTransactionId, keys.values()))
                .stream().collect(Collectors.toMap(WechatOrderSync::getTransactionId, Function.identity()));
        for (WechatOrderVO row : result.getRecords()) {
            String key = keys.get(row.getSn());
            WechatOrderSync state = key == null ? null : states.get(key);
            row.setWechatSyncStatus(state == null ? "UNCHECKED" : state.getStatus());
            row.setWechatSyncTime(state == null ? null : state.getLastAttemptTime());
            row.setWechatSyncError(state == null ? null : state.getErrorMessage());
        }
        return result;
    }

    /** 按支付流水分页查询日志；列表由触发子订单进入，同笔支付的其他子订单也可审计。 */
    public IPage<WechatOrderSyncLog> logs(Order order, int pageNumber, int pageSize) {
        return logMapper.selectPage(new Page<>(Math.max(1, pageNumber), Math.max(1, Math.min(100, pageSize))),
                new LambdaQueryWrapper<WechatOrderSyncLog>().and(scope -> scope
                                .eq(WechatOrderSyncLog::getTransactionId, transactionKey(order))
                                .or().eq(WechatOrderSyncLog::getOrderSn, order.getSn()))
                        .orderByDesc(WechatOrderSyncLog::getCreateTime).orderByDesc(WechatOrderSyncLog::getId));
    }

    /** 管理接口的服务端角色边界，不允许买家或商家借专用接口补报其他店铺订单。 */
    public void requireManager(AuthUser user) {
        if (user == null || user.getRole() != UserEnums.MANAGER) throw new ServiceException("仅平台管理员可操作微信订单同步");
    }
}
