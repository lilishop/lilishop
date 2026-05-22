package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.GiftCardCashActivity;
import cn.lili.modules.promotion.entity.dos.GiftCardCashCreateBatch;
import cn.lili.modules.promotion.entity.dos.GiftCardCashIssueBatch;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.dto.GiftCardCashBatchCreateDTO;
import cn.lili.modules.promotion.entity.dto.GiftCardCashDeductPreviewDTO;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashActivitySearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCreateBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashCredentialSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashIssueBatchSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardCashMemberCardSearchParams;
import cn.lili.modules.promotion.entity.dto.search.GiftCardUsageRecordSearchParams;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCredentialVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashCreateBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashDeductPreviewVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashIssueBatchVO;
import cn.lili.modules.promotion.entity.vos.GiftCardCashMemberCardVO;
import cn.lili.modules.promotion.entity.vos.GiftCardUsageRecordVO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.aftersale.entity.dos.AfterSale;
import com.baomidou.mybatisplus.core.metadata.IPage;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.multipart.MultipartFile;

/**
 * 现金礼品卡服务（活动、制卡、发卡、凭证查询与导出）。
 * <p>
 * 说明：
 * <ul>
 *   <li>活动是规则与库存的载体；</li>
 *   <li>制卡在活动下批量生成未发凭证（可导出卡密、用户自助绑定）；</li>
 *   <li>发卡通过导入手机号+数量，直接生成凭证并发放给会员（与制卡独立，共用活动库存）；</li>
 *   <li>发送记录用于追踪每次导入发卡批次。</li>
 * </ul>
 *
 * @author Bulbasaur
 * @since 2026-05-22
 */
public interface GiftCardCashService {

    /**
     * 新建礼品卡活动。
     *
     * @param dto 活动参数（名称、面额、有效期规则、总库存等）
     * @return 活动ID
     */
    String saveActivity(GiftCardCashActivityDTO dto);

    /**
     * 更新礼品卡活动。
     *
     * @param id  活动ID
     * @param dto 更新参数
     */
    void updateActivity(String id, GiftCardCashActivityDTO dto);

    /**
     * 删除礼品卡活动（当活动下不存在制卡/凭证数据时允许）。
     *
     * @param id 活动ID
     */
    void deleteActivity(String id);

    /**
     * 查询活动详情。
     *
     * @param id 活动ID
     * @return 活动详情（含聚合字段）
     */
    GiftCardCashActivity getActivity(String id);

    /**
     * 活动分页查询。
     *
     * @param params 查询条件
     * @param page   分页参数
     * @return 活动分页结果（含制卡数/发卡数/剩余库存聚合）
     */
    IPage<GiftCardCashActivity> pageActivity(GiftCardCashActivitySearchParams params, PageVO page);

    /**
     * 在活动下批量制卡（写入凭证，占用活动库存上限）。
     *
     * @param activityId 活动ID
     * @param dto        制卡参数（数量、批次备注）
     * @return 制卡批次ID
     */
    String batchCreateCards(String activityId, GiftCardCashBatchCreateDTO dto);

    /**
     * 制卡批次分页查询。
     *
     * @param params 查询条件
     * @param page   分页参数
     * @return 制卡批次分页结果
     */
    IPage<GiftCardCashCreateBatchVO> pageCreateBatch(GiftCardCashCreateBatchSearchParams params, PageVO page);

    /**
     * 卡密凭证分页查询（含会员绑定摘要）。
     *
     * @param params 查询条件
     * @param page   分页参数
     * @return 凭证分页结果
     */
    IPage<GiftCardCashCredentialVO> pageCredential(GiftCardCashCredentialSearchParams params, PageVO page);

    /**
     * 发卡记录分页查询。
     *
     * @param params 查询条件
     * @param page   分页参数
     * @return 发卡记录分页结果
     */
    IPage<GiftCardCashIssueBatchVO> pageIssueBatch(GiftCardCashIssueBatchSearchParams params, PageVO page);

    /**
     * 礼品卡使用记录分页查询。
     *
     * @param params 查询条件
     * @param page   分页参数
     * @return 礼品卡使用记录分页结果
     */
    IPage<GiftCardUsageRecordVO> pageUsageRecord(GiftCardUsageRecordSearchParams params, PageVO page);

    /**
     * 导出制卡批次卡号卡密。
     *
     * @param createBatchId 制卡批次ID
     * @param response      HTTP响应
     */
    void exportCreateBatch(String createBatchId, HttpServletResponse response);

    /**
     * 下载发卡导入模板（Excel）。
     *
     * @param response HTTP响应
     */
    void downloadIssueTemplate(HttpServletResponse response);

    /**
     * 按活动导入发卡（直接生成凭证并绑定会员，扣减活动库存）。
     *
     * @param activityId 活动ID
     * @param file       导入文件（手机号、发卡数量）
     * @return 发卡记录ID
     */
    String issueImportByActivity(String activityId, MultipartFile file);

    /**
     * 买家端分页查询当前会员礼品卡。
     *
     * @param memberId 会员ID
     * @param params   查询条件（可用/不可用/待激活）
     * @param page     分页参数
     * @return 会员礼品卡分页结果
     */
    IPage<GiftCardCashMemberCardVO> pageMemberCards(String memberId, GiftCardCashMemberCardSearchParams params, PageVO page);

    /**
     * 买家端绑定礼品卡。
     *
     * @param memberId   会员ID
     * @param cardNo     卡号
     * @param cardSecret 兑换码/卡密
     */
    void bindMemberCard(String memberId, String cardNo, String cardSecret);

    /**
     * 买家端激活礼品卡（绑定后可激活）。
     *
     * @param memberId      会员ID
     * @param memberCardId  会员礼品卡ID（凭证ID）
     */
    void activateMemberCard(String memberId, String memberCardId);

    /**
     * 买家端查询当前会员可用礼品卡（用于下单页展示）。
     *
     * @param memberId 会员ID
     * @return 可用礼品卡列表
     */
    java.util.List<GiftCardCashMemberCardVO> listAvailableMemberCards(String memberId);

    /**
     * 买家端礼品卡抵扣试算（支持多卡叠加）。
     *
     * @param memberId 会员ID
     * @param dto      试算参数
     * @return 试算结果
     */
    GiftCardCashDeductPreviewVO previewMemberCardDeduction(String memberId, GiftCardCashDeductPreviewDTO dto);

    /**
     * 下单时按试算结果真实扣减礼品卡余额。
     *
     * @param memberId    会员ID
     * @param deductItems 抵扣明细
     */
    void deductMemberCardBalance(String memberId, java.util.List<GiftCardCashDeductPreviewVO.DeductItem> deductItems);

    /**
     * 售后退款时按订单礼品卡记录回退余额。
     *
     * @param afterSale 售后单
     * @return 本次回卡金额
     */
    java.math.BigDecimal refundMemberCardBalance(AfterSale afterSale);

    /**
     * 售后退款时按指定上限回退礼品卡余额。
     *
     * @param afterSale    售后单
     * @param refundLimit  本次允许回卡上限（通常为该售后按分摊计算出的礼品卡部分）
     * @return 本次实际回卡金额
     */
    java.math.BigDecimal refundMemberCardBalance(AfterSale afterSale, java.math.BigDecimal refundLimit);

    /**
     * 订单整单退款时按上限回退礼品卡余额。
     *
     * @param orderSn     订单号
     * @param refundLimit 本次允许回卡上限
     * @return 本次实际回卡金额
     */
    java.math.BigDecimal refundOrderMemberCardBalance(String orderSn, java.math.BigDecimal refundLimit);

    /**
     * 取消订单退款拆分：优先回卡，返回渠道应退金额。
     *
     * @param orderSn     订单号
     * @param orderAmount 订单应退总金额
     * @return 渠道应退金额
     */
    java.math.BigDecimal resolveChannelRefundAfterGiftCard(String orderSn, java.math.BigDecimal orderAmount);

    /**
     * 售后退款拆分：优先回卡，返回渠道应退金额。
     *
     * @param afterSale 售后单
     * @return 渠道应退金额
     */
    java.math.BigDecimal resolveChannelRefundAfterGiftCardForAfterSale(AfterSale afterSale);

    /**
     * 下单时应用礼品卡分摊并生成礼品卡使用记录（扣减流水）。
     *
     * @param tradeDTO   交易对象
     * @param orders     订单集合
     * @param orderItems 订单项集合
     */
    void applyGiftCardUsageOnOrderCreate(TradeDTO tradeDTO, java.util.List<Order> orders, java.util.List<OrderItem> orderItems);

    /**
     * 将已过期的礼品卡凭证批量置为 EXPIRED（定时任务调用）。
     *
     * @return 本次更新的凭证数量
     */
    int expireOverdueCredentials();
}
