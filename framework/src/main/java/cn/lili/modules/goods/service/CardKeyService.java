package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dto.CardKeyAddDTO;
import cn.lili.modules.goods.entity.dto.CardKeyImportDTO;
import cn.lili.modules.goods.entity.dto.search.CardKeySearchParams;
import cn.lili.modules.goods.entity.vo.CardKeyImportResultVO;
import cn.lili.modules.goods.entity.vo.CardKeyPoolVO;
import cn.lili.modules.goods.entity.vo.CardKeyStatsVO;
import cn.lili.modules.goods.entity.vo.CardKeyVO;
import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Order;
import cn.lili.modules.order.order.entity.dos.OrderItem;
import cn.lili.modules.order.order.entity.vo.OrderDetailVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

/**
 * 电子卡券（卡密商品）核心服务。
 * <p>
 * 职责：卡池 CRUD、库存同步、支付后发卡履约、下单/结算规则校验、订单详情卡密填充。
 *
 * @author Mike
 * @date 2026-07-31
 */
public interface CardKeyService {

    /**
     * Excel 批量导入卡密。
     * 部分失败时成功行入库并返回 failRows，不整体回滚。
     */
    CardKeyImportResultVO importExcel(String skuId, MultipartFile file);

    /**
     * 单条新增卡密，成功后同步可售库存。
     */
    CardKeyPoolVO addCardKey(CardKeyAddDTO dto);

    /**
     * 卡池分页列表，按店铺隔离，返回明文 cardSecret 供商家管理页展示。
     */
    IPage<CardKeyPoolVO> page(CardKeySearchParams params);

    /**
     * 作废卡密，仅 UNUSED 可作废，作废后 syncSkuStock。
     */
    void voidCard(String id);

    /**
     * 卡池各状态数量统计。
     */
    CardKeyStatsVO stats(String skuId);

    /**
     * 按筛选条件同步导出卡池 Excel，单次上限 10,000 行。
     */
    void export(CardKeySearchParams params, HttpServletResponse response);

    /**
     * 下载卡密导入模板，仅含「卡号」「卡密」表头。
     */
    void downloadImportTemplate(HttpServletResponse response);

    /**
     * 批量导入卡密，供 importExcel / addCardKey 内部复用。
     */
    CardKeyImportResultVO importBatch(String skuId, List<CardKeyImportDTO> rows);

    /**
     * 同步 SKU 可售库存：countUnusedPoolStock → li_goods_sku.quantity，并汇总至 li_goods.quantity。
     */
    void syncSkuStock(String skuId);

    /**
     * 统计 SKU 可售卡池数量（status=UNUSED 且 delete_flag=0），用于 syncSkuStock 与下单前实时校验。
     */
    int countUnusedPoolStock(String skuId);

    /**
     * 支付成功后从卡池分配卡密并绑定订单（购买型 / 满赠子单）。
     */
    void allocateByOrder(String orderSn);

    /**
     * 拼团 PAID 后预占卡密（UNUSED → RESERVED）。
     */
    void reserveByOrder(String orderSn);

    /**
     * 拼团成团：RESERVED → ALLOCATED 并完成订单。
     */
    void confirmReservationAndComplete(String orderSn);

    /**
     * 拼团失败等：释放 RESERVED 回 UNUSED。
     */
    void releaseReservation(String orderSn);

    /**
     * 电子卡券支付后履约入口（购买 / 拼团预占 / 满赠子单路由）。
     */
    void fulfillAfterPayment(String orderSn);

    /**
     * 发卡失败或系统异常退款前，将本单已 ALLOCATED 的卡密标记为 VOIDED，不回收到 UNUSED。
     */
    void voidOnSystemFailure(String orderSn);

    /**
     * 商品/SKU 软删除时，软删该 SKU 下 UNUSED/VOIDED 卡池行，ALLOCATED 保留。
     */
    void softDeletePoolOnSkuRemove(String skuId);

    /**
     * 是否为可走卡池的购买型 E_COUPON 订单（NORMAL / POINTS / KANJIA，且无 parentOrderSn）。
     */
    boolean isPurchaseTypeOrder(Order order);

    /**
     * 是否为满赠 E_COUPON 赠品子单。
     */
    boolean isGiftECouponOrder(Order order);

    /**
     * 是否为拼团 E_COUPON 订单。
     */
    boolean isPintuanECouponOrder(Order order);

    /**
     * E_COUPON 支付后履约门禁：orderType=E_COUPON、payStatus=PAID、orderStatus=PAID。
     */
    boolean isECouponFulfillmentEligible(Order order);

    /**
     * 当前交易是否全部为 E_COUPON SKU，用于选择专用渲染链路与免地址逻辑。
     */
    boolean isECouponOnlyTrade(TradeDTO tradeDTO);

    /**
     * 归一化 E_COUPON 交易：免地址、虚拟发货、freight=0（不清除促销选择）。
     */
    void normalizeECouponTrade(TradeDTO tradeDTO);

    /**
     * 创建交易前校验：禁混单、禁多 SKU、禁 CART、卡池库存、关店等。
     */
    void validateTradeForECoupon(TradeDTO tradeDTO);

    /**
     * 加购/立即购买前校验：E_COUPON 禁止加入购物车，实时校验卡池库存与关店禁售。
     */
    void validateAddToCart(String skuId, Integer num, String cartType);

    /**
     * 支付前校验：关店后禁止 E_COUPON 新单支付。
     */
    void validatePayOrder(Order order);

    /**
     * 填充买卖家订单详情：orderItems 卡密/履约状态；主单聚合满赠 E_COUPON 子单 {@link OrderDetailVO#getGiftECouponOrders()}。
     */
    void enrichOrderDetail(OrderDetailVO orderDetailVO);

    /**
     * 按订单项查询已分配卡密 VO 列表，解密 cardSecret；includeStatus 为 true 时含状态字段（商家端）。
     */
    List<CardKeyVO> listCardKeyVOByOrderItemSn(String orderItemSn, boolean includeStatus);
}
