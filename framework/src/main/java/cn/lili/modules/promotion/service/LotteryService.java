package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.*;
import com.baomidou.mybatisplus.core.metadata.IPage;
import java.util.Map;

/**
 * 平台每日免费抽奖业务接口，统一活动规则、次数、发奖及幂等校验。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
public interface LotteryService {
    /**
     * 分页查询抽奖活动及奖池，买家仅查询已上架且在有效期内的活动。
     *
     * @param page 分页参数
     * @param buyer 是否为买家查询；true返回公开奖池，false返回管理端完整配置
     * @return 活动及奖池分页
     */
    IPage<LotteryActivity> activities(PageVO page, boolean buyer);

    /**
     * 创建或修改未发布草稿，统一校验活动规则、奖项、概率和优惠券资格。
     * 已发布活动禁止修改；活动与奖池保存失败时整体回滚。
     *
     * @param input 活动及奖项配置；无ID时新建，有ID时编辑草稿
     * @return 保存后的活动及奖池
     */
    LotteryActivity configure(LotteryActivity input);

    /**
     * 上下架活动，首次上架后冻结配置；下架保留奖励及记录，活动结束后禁止再次上架。
     *
     * @param id 抽奖活动ID
     * @param enabled true上架、false下架
     */
    void status(String id, boolean enabled);

    /**
     * 查询会员在指定活动下的北京时间当天次数；实际抽奖资格由发奖事务再次校验。
     *
     * @param id 抽奖活动ID
     * @param member 当前登录会员ID
     * @return day北京时间日期、used已用次数、remaining剩余次数
     */
    Map<String, Object> chances(String id, String member);

    /**
     * 免费抽奖，次数、库存、发奖与记录同事务提交，失败整体回滚。
     * 同一会员重复请求返回原结果，不重复发奖；请求号不得跨活动复用。
     *
     * @param id 抽奖活动ID
     * @param memberId 当前登录会员ID
     * @param requestId 会员内唯一请求号，1至64位字母、数字或横线；网络重试必须复用
     * @return 已完成的抽奖及发奖结果，包含未中奖；重复请求返回原结果
     */
    LotteryRecord draw(String id, String memberId, String requestId);

    /**
     * 分页查询已完成抽奖记录，包含中奖及未中奖结果；活动下架或结束不影响查询。
     *
     * @param page 分页参数
     * @param memberId 会员ID；买家必须传入当前登录会员ID，管理查询可为空
     * @param activityId 可选抽奖活动ID，为空时不按活动筛选
     * @return 已完成抽奖记录分页
     */
    IPage<LotteryRecord> records(PageVO page, String memberId, String activityId);
}
