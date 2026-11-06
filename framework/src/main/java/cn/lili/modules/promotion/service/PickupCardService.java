package cn.lili.modules.promotion.service;

import cn.lili.modules.order.cart.entity.dto.TradeDTO;
import cn.lili.modules.order.order.entity.dos.Trade;
import cn.lili.modules.promotion.entity.dto.GiftCardCashActivityDTO;
import cn.lili.modules.promotion.entity.dto.PickupCardRedeemDTO;
import cn.lili.modules.promotion.entity.vos.PickupCardDetailVO;
import cn.lili.modules.promotion.entity.vos.PickupCardSkuVO;

import java.util.List;

/**
 * 提货卡活动规则校验、会员选品试算及一次性兑换服务，复用标准交易、订单和物流能力。
 * <p>
 * 支持任选一种（ONE）、全部兑换（ALL）及任选指定种数（ANY），每种 SKU 兑换一件。
 * 兑换按下单时商品销售价结算，买家无需支付且免运费；订单取消不恢复卡权益，不提供退款、退货能力。
 * 同一卡的兑换须保证并发互斥和重复请求幂等，卡归属校验必须先于原交易返回。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
public interface PickupCardService {
    /**
     * 校验提货活动的兑换模式、候选 SKU 和兑换种数，并原地归一化活动参数。
     * <p>
     * 候选范围为 1～50 个不同的、审核通过且已上架的普通实物 SKU；ONE 模式固定兑换一种，
     * ALL 模式兑换全部候选种类，ANY 模式的种数须在候选范围内。提货卡面额归零，不作为储值余额使用。
     *
     * @param dto 待保存的活动参数，不能为空；校验成功后更新其中的兑换种数和面额
     * @throws cn.lili.common.exception.ServiceException 兑换规则无效或候选商品不符合要求时抛出
     */
    void validateActivity(GiftCardCashActivityDTO dto);

    /**
     * 按活动配置的 SKU 顺序生成展示数据，供管理端回显及买家选品。
     * <p>
     * 失效商品保留原 SKU 的占位信息，不替换成其他商品；商品状态和当前库存共同决定是否可选。
     * 返回数据不包含卡密等凭据或内部成本字段。
     *
     * @param skuIds 候选 SKU ID 的 JSON 字符串数组；为空时返回空列表
     * @return 与配置顺序一致的商品展示列表，包含当前可选状态
     */
    List<PickupCardSkuVO> goods(String skuIds);

    /**
     * 查询指定会员名下的提货卡及其活动兑换规则，不返回卡密。
     * <p>
     * 校验卡归属、卡类型及活动是否存在；详情查询本身不代表卡已满足兑换条件，兑换资格在试算和提交时重新校验。
     *
     * @param memberId     当前登录会员 ID，由调用方从登录上下文取得
     * @param credentialId 提货卡凭据 ID，对应礼品卡凭据记录
     * @return 卡标识、兑换规则、候选商品及已关联交易号
     * @throws cn.lili.common.exception.ServiceException 卡不存在、不属于该会员、类型不符或活动不存在时抛出
     */
    PickupCardDetailVO detail(String memberId, String credentialId);

    /**
     * 根据服务端最新商品、卡资格和会员收货地址试算兑换交易，不占库存、不核销权益。
     * <p>
     * 校验激活状态、有效期、选品种数、商品库存及配送范围，按商品销售价计算结算金额；
     * 免运费，且不执行促销、会员价、赠品、积分或分销。试算成功不保证提交时仍有库存。
     *
     * @param memberId 当前登录会员 ID，须与卡归属及收货地址归属一致
     * @param dto      兑换请求，包含卡凭据 ID、所选 SKU ID 和会员收货地址 ID，不能为空
     * @return 服务端生成的交易试算数据，金额沿用交易模型的人民币元口径
     * @throws cn.lili.common.exception.ServiceException 卡资格、选品、地址、库存或配送校验失败时抛出
     */
    TradeDTO preview(String memberId, PickupCardRedeemDTO dto);

    /**
     * 一次性核销提货卡，按店铺创建订单并以无需实际支付的方式完成交易支付。
     * <p>
     * 对同一卡加行锁串行处理，先校验归属，再对已兑换的卡返回原交易，不重复扣库存或创单。
     * 首次兑换重新执行资格与选品校验，在同一数据库事务内更新卡状态、创建订单、记录抵扣流水并完成支付；
     * 数据库操作失败时回滚，已扣 Redis 库存通过事务回调补偿。成功兑换后整卡权益耗尽，取消订单也不恢复。
     *
     * @param memberId 当前登录会员 ID，由调用方从登录上下文取得
     * @param dto      兑换请求，包含卡凭据 ID、所选 SKU ID 和收货地址 ID，不能为空
     * @return 本次兑换创建的交易；同一卡成功兑换后的重复请求返回原交易
     * @throws cn.lili.common.exception.ServiceException 卡资格或选品校验失败、库存不足或卡状态更新冲突时抛出
     */
    Trade redeem(String memberId, PickupCardRedeemDTO dto);

    /**
     * 恢复已取消提货订单的商品库存，永不恢复卡权益。
     * <p>
     * 仅处理存在 PICKUP_REDEEM 流水且状态为 CANCELLED 的订单；锁定原流水并检查取消处理记录保证重复消息幂等。
     * PICKUP_CANCEL 零金额记录与数据库库存更新处于同一事务，Redis 库存通过事务回调在回滚时补偿。
     * 非提货订单、未取消订单或已释放库存的订单不再处理。
     *
     * @param order 已取消订单的详情，须包含订单及其明细，不能为空
     * @throws cn.lili.common.exception.ServiceException Redis 或数据库库存恢复失败时抛出
     */
    void releaseCancelledStock(cn.lili.modules.order.order.entity.vo.OrderDetailVO order);
}
