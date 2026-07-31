/**
 * 作者：Mike
 * 日期：2026-07-31
 */
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
 * 需求依据：card-key-goods-v4.md；内部服务编号 SVC-01~SVC-07。
 *
 * @author Mike
 * @date 2026-07-31
 */
public interface CardKeyService {

    /**
     * Excel 批量导入卡密（API-S-01）。
     * 部分失败时成功行入库并返回 failRows，不整体回滚。
     */
    CardKeyImportResultVO importExcel(String skuId, MultipartFile file);

    /**
     * 单条新增卡密（API-S-02），成功后同步可售库存。
     */
    CardKeyPoolVO addCardKey(CardKeyAddDTO dto);

    /**
     * 卡池分页列表（API-S-03），按店铺隔离，返回明文 cardSecret 供商家管理页展示。
     */
    IPage<CardKeyPoolVO> page(CardKeySearchParams params);

    /**
     * 作废卡密（API-S-04），仅 UNUSED 可作废，作废后 syncSkuStock。
     */
    void voidCard(String id);

    /**
     * 卡池各状态数量统计（API-S-05）。
     */
    CardKeyStatsVO stats(String skuId);

    /**
     * 按筛选条件同步导出卡池 Excel（API-S-07），单次上限 10,000 行。
     */
    void export(CardKeySearchParams params, HttpServletResponse response);

    /**
     * 下载卡密导入模板（API-S-01a），仅含「卡号」「卡密」表头。
     */
    void downloadImportTemplate(HttpServletResponse response);

    /**
     * 批量导入卡密（SVC-04），供 importExcel / addCardKey 内部复用。
     */
    CardKeyImportResultVO importBatch(String skuId, List<CardKeyImportDTO> rows);

    /**
     * 同步 SKU 可售库存：poolStock → li_goods_sku.quantity，并汇总至 li_goods.quantity（SVC-02）。
     */
    void syncSkuStock(String skuId);

    /**
     * 统计 SKU 可售卡池数量（status=UNUSED 且 delete_flag=0），用于下单前库存校验与前端 poolStock 展示。
     */
    int countUnusedPoolStock(String skuId);

    /**
     * 支付成功后从卡池分配卡密并绑定订单（SVC-01）。
     * 仅购买型 E_COUPON（非满赠子单）；FOR UPDATE 取卡，整单原子，先导入先发。
     */
    void allocateByOrder(String orderSn);

    /**
     * 电子卡券支付后履约入口（SVC-07）。
     * 购买型：allocateByOrder → COMPLETED；失败则 voidOnSystemFailure + 系统退款。
     * 非购买型（满赠子单等）：直接 COMPLETED，不扣卡池。
     */
    void fulfillAfterPayment(String orderSn);

    /**
     * 发卡失败或系统异常退款前，将本单已 ALLOCATED 的卡密标记为 VOIDED，不回收到 UNUSED（SVC-03 / O-08）。
     */
    void voidOnSystemFailure(String orderSn);

    /**
     * 商品/SKU 软删除时，软删该 SKU 下 UNUSED/VOIDED 卡池行，ALLOCATED 保留（SVC-05 / S-06）。
     */
    void softDeletePoolOnSkuRemove(String skuId);

    /**
     * 店铺关店时，批量软删全店 E_COUPON SKU 的未售/作废卡池（SVC-06 / S-07）。
     */
    void softDeletePoolOnStoreClose(String storeId);

    /**
     * 是否为可走卡池的购买型 E_COUPON 订单：orderType=E_COUPON 且非 GIFT 且无 parentOrderSn（G-01 / EC-08）。
     */
    boolean isPurchaseTypeOrder(Order order);

    /**
     * 当前交易是否全部为 E_COUPON SKU，用于选择专用渲染链路与免地址逻辑（O-11）。
     */
    boolean isECouponOnlyTrade(TradeDTO tradeDTO);

    /**
     * 归一化 E_COUPON 交易：清除地址/优惠券/满减选择，deliveryMethod=VIRTUAL，freight=0（O-11 / EC-26）。
     */
    void normalizeECouponTrade(TradeDTO tradeDTO);

    /**
     * 创建交易前校验：禁混单、禁多 SKU、禁促销、poolStock、flowPrice&gt;0 等（FR-B-01 / EC-14~20 / O-12a）。
     */
    void validateTradeForECoupon(TradeDTO tradeDTO);

    /**
     * 加购/立即购买前校验：E_COUPON 禁止加入购物车，校验 poolStock 与关店禁售。
     */
    void validateAddToCart(String skuId, Integer num, String cartType);

    /**
     * 支付前校验：关店后禁止 E_COUPON 新单支付（S-07 / EC-23）。
     */
    void validatePayOrder(Order order);

    /**
     * 填充买卖家订单详情 orderItems[].cardKeys 与 cardKeyDelivered（API-B-01 / §6.2）。
     */
    void enrichOrderDetail(OrderDetailVO orderDetailVO);

    /**
     * 按订单项查询已分配卡密 VO 列表，解密 cardSecret；includeStatus 为 true 时含状态字段（商家端）。
     */
    List<CardKeyVO> listCardKeyVOByOrderItemSn(String orderItemSn, boolean includeStatus);
}
