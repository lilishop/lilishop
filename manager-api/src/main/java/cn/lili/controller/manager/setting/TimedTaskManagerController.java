package cn.lili.controller.manager.setting;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.vo.ResultMessage;
import cn.lili.timetask.handler.EveryDayExecute;
import cn.lili.timetask.handler.EveryHourExecute;
import cn.lili.timetask.handler.EveryMinuteExecute;
import cn.lili.timetask.handler.impl.order.OrderEveryDayTaskExecute;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

import java.util.*;

/**
 * 管理端,定时任务管理接口
 */
@Slf4j
@RestController
@Tag(name = "管理端,定时任务管理接口")
@RequestMapping("/manager/setting/timed-task")
public class TimedTaskManagerController {

    @Autowired
    private List<EveryMinuteExecute> everyMinuteExecutes;

    @Autowired
    private List<EveryHourExecute> everyHourExecutes;

    @Autowired
    private List<EveryDayExecute> everyDayExecutes;

    @Autowired
    private OrderEveryDayTaskExecute orderEveryDayTaskExecute;

    private static final Map<String, String> NAMES = new HashMap<>();
    static {
        // 每分钟
        NAMES.put("CancelOrderTaskExecute",         "订单自动取消");
        NAMES.put("RechargeOrderTaskExecute",        "充值订单处理");
        // 每小时
        NAMES.put("BroadcastExecute",               "广播推送");
        NAMES.put("CouponExecute",                  "优惠券状态更新");
        NAMES.put("GoodsExecute",                   "商品状态更新");
        NAMES.put("OnlineMemberStatistics",         "在线会员统计");
        NAMES.put("PageViewStatisticsExecute",      "页面访问统计");
        NAMES.put("StoreExecute",                   "店铺状态处理");
        NAMES.put("StoreRatingExecute",             "店铺评分更新");
        NAMES.put("MemberStatisticsExecute",        "会员统计");
        NAMES.put("GiftCardCashExpireExecute",      "礼品卡现金过期处理");
        NAMES.put("PromotionEverydayExecute",       "促销活动每日处理");
        NAMES.put("MemberCouponSignEverydayExecute","会员优惠券签到");
        // 每日（其他）
        NAMES.put("HotWordsEveryDayTaskExecute",    "热词每日更新");
        NAMES.put("DistributionRelationExpireExecute","分销关系过期处理");
    }

    @Operation(summary = "获取定时任务列表（每个任务单独一行）")
    @GetMapping
    public ResultMessage<List<Map<String, Object>>> list() {
        List<Map<String, Object>> tasks = new ArrayList<>();

        for (EveryMinuteExecute t : everyMinuteExecutes) {
            String id = AopUtils.getTargetClass(t).getSimpleName();
            tasks.add(row(id, NAMES.getOrDefault(id, id), "每分钟", "0 * * * * ?"));
        }

        for (EveryHourExecute t : everyHourExecutes) {
            String id = AopUtils.getTargetClass(t).getSimpleName();
            tasks.add(row(id, NAMES.getOrDefault(id, id), "每小时", "0 0 * * * ?"));
        }

        // OrderEveryDayTaskExecute 内部拆分为独立子任务，逐个列出
        tasks.add(row("ORDER_AUTO_COMPLETE",   "自动确认收货",          "每日任务", "0 0 0 * * ?"));
        tasks.add(row("ORDER_AUTO_EVALUATE",   "自动好评",             "每日任务", "0 0 0 * * ?"));
        tasks.add(row("ORDER_CLOSE_AFTER_SALE","关闭售后申请",          "每日任务", "0 0 0 * * ?"));
        tasks.add(row("ORDER_CLOSE_COMPLAINT", "关闭投诉申请",          "每日任务", "0 0 0 * * ?"));
        tasks.add(row("ORDER_PROFIT_SHARING",  "订单分账（解冻佣金）",   "每日任务", "0 0 0 * * ?"));
        tasks.add(row("ORDER_CREATE_BILL",     "生成店铺结算单",         "每日任务", "0 0 0 * * ?"));

        // 其余 EveryDayExecute 实现（跳过 OrderEveryDayTaskExecute，已拆分列出）
        for (EveryDayExecute t : everyDayExecutes) {
            if (t instanceof OrderEveryDayTaskExecute) {
                continue;
            }
            String id = AopUtils.getTargetClass(t).getSimpleName();
            tasks.add(row(id, NAMES.getOrDefault(id, id), "每日任务", "0 0 0 * * ?"));
        }

        return ResultUtil.data(tasks);
    }

    @Operation(summary = "手动触发指定任务")
    @PostMapping("/trigger/{taskId}")
    public ResultMessage<Object> trigger(@PathVariable String taskId) {
        log.info("手动触发任务: {}", taskId);
        try {
            // OrderEveryDayTaskExecute 子任务
            switch (taskId) {
                case "ORDER_AUTO_COMPLETE":    orderEveryDayTaskExecute.executeAutoComplete();   return ok();
                case "ORDER_AUTO_EVALUATE":    orderEveryDayTaskExecute.executeAutoEvaluate();   return ok();
                case "ORDER_CLOSE_AFTER_SALE": orderEveryDayTaskExecute.executeCloseAfterSale(); return ok();
                case "ORDER_CLOSE_COMPLAINT":  orderEveryDayTaskExecute.executeCloseComplaint(); return ok();
                case "ORDER_PROFIT_SHARING":   orderEveryDayTaskExecute.executeProfitSharing();  return ok();
                case "ORDER_CREATE_BILL":      orderEveryDayTaskExecute.executeCreateBill();     return ok();
            }

            // 在三个列表中按类名查找并执行
            for (EveryMinuteExecute t : everyMinuteExecutes) {
                if (AopUtils.getTargetClass(t).getSimpleName().equals(taskId)) {
                    t.execute();
                    return ok();
                }
            }
            for (EveryHourExecute t : everyHourExecutes) {
                if (AopUtils.getTargetClass(t).getSimpleName().equals(taskId)) {
                    t.execute();
                    return ok();
                }
            }
            for (EveryDayExecute t : everyDayExecutes) {
                if (AopUtils.getTargetClass(t).getSimpleName().equals(taskId)) {
                    t.execute();
                    return ok();
                }
            }

            return ResultUtil.error(ResultCode.ERROR.code(), "未知任务ID: " + taskId);
        } catch (Exception e) {
            log.error("手动触发任务失败: {}", taskId, e);
            return ResultUtil.error(ResultCode.ERROR.code(), "执行失败: " + e.getMessage());
        }
    }

    private ResultMessage<Object> ok() {
        return ResultUtil.success();
    }

    private Map<String, Object> row(String id, String name, String group, String cron) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("id", id);
        m.put("name", name);
        m.put("group", group);
        m.put("cron", cron);
        return m;
    }
}
