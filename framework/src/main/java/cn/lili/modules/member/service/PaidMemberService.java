package cn.lili.modules.member.service;

import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.PaidMemberOrder;
import cn.lili.modules.system.entity.dto.PaidMemberSetting;
import com.baomidou.mybatisplus.core.metadata.IPage;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

/**
 * 单一付费会员业务接口，提供统一配置、购买及续费订单、有效资格和商品折扣查询。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
public interface PaidMemberService {

    /**
     * 读取 li_setting 的单一会员配置，未配置或空值时返回默认关闭的设置。
     *
     * @return 当前平台配置
     */
    PaidMemberSetting config();

    /**
     * 生成配置 JSON 的 SHA-256 摘要，用于购买确认及订单快照追溯。
     *
     * @param config 完整会员配置
     * @return 配置版本摘要
     */
    String version(PaidMemberSetting config);

    /**
     * 检查权益、商家和 SKU 存在性以及折扣商品商家归属，清理富文本后保存统一设置。
     * 推荐商品只校验存在性，不限制为参与优惠的商家；保存失败回滚。
     *
     * @param config 平台提交的完整配置
     * @throws ServiceException 关联数据、业务规则非法或保存失败
     */
    void saveConfig(PaidMemberSetting config);

    /**
     * 查询已支付且当前处于 [startTime, endTime) 区间内的会员订单。
     *
     * @param memberId 客户 ID，空值返回 null
     * @return 当前有效订单，无有效资格时返回 null
     */
    PaidMemberOrder active(String memberId);

    /**
     * 解析订单购买时的完整配置快照，后续平台修改不追溯改变已购折扣规则。
     *
     * @param order 会员购买订单
     * @return 成交配置快照
     */
    PaidMemberSetting snapshot(PaidMemberOrder order);

    /**
     * 读取当前有效订单关联且仍处于开启状态的客户权益。
     *
     * @param memberId 客户 ID
     * @return 可用权益，无有效会员时为空列表
     */
    List<MemberBenefit> activeBenefits(String memberId);

    /**
     * 汇总会员中心配置、资格、到期时间、权益及推荐商品。
     * 节省金额按客户跨开通/续费周期累计统计并扣除退款冲回，推荐商品仅返回已上架且审核通过商品的买家展示字段。
     *
     * @param memberId 客户 ID
     * @return 展示数据，状态为 ACTIVE、UNOPENED 或 EXPIRED
     */
    Map<String, Object> current(String memberId);

    /**
     * 在客户行锁内按客户及请求编号复用订单，新建前校验协议与配置版本。
     * 新订单保存价格、自然月期限及配置快照，支付期限为创建后 30 分钟。
     *
     * @param memberId 购买客户 ID
     * @param requestId 8～64 位字母、数字、下划线或连字符组成的幂等编号
     * @param configVersion 客户确认购买时的配置版本
     * @param agreed 是否明确同意会员协议
     * @return 已存在或新建的购买记录，重复请求沿用原记录状态
     * @throws ServiceException 客户不存在、未同意协议、配置变更或暂停购买
     */
    PaidMemberOrder create(String memberId, String requestId, String configVersion, Boolean agreed);

    /**
     * 按业务单号读取会员订单，此方法不校验客户归属。
     *
     * @param sn PM 前缀会员订单号
     * @return 会员订单
     * @throws ServiceException 订单不存在
     */
    PaidMemberOrder order(String sn);

    /**
     * 读取并校验订单归属，防止客户访问他人购买记录。
     *
     * @param memberId 当前客户 ID
     * @param sn 会员订单号
     * @return 当前客户的订单
     * @throws ServiceException 订单不存在或归属不匹配
     */
    PaidMemberOrder ownedOrder(String memberId, String sn);

    /**
     * 锁定并取消当前客户的未支付订单，已支付订单不可取消。
     *
     * @param memberId 当前客户 ID
     * @param sn 会员订单号
     * @throws ServiceException 订单不存在、归属不符或不是 UNPAID 状态
     */
    void cancel(String memberId, String sn);

    /**
     * 处理已经渠道核验的真实到账，依次锁定订单及客户，防止重复开通和并发续费重叠。
     * 核对金额及渠道后，已支付订单直接返回；续费从尚未到期的末次结束时间起算。
     * 取消或超时与真实到账竞态时仍承认到账一次；权益在事务提交后发放，失败交由周期任务重试。
     *
     * @param sn 会员订单号
     * @param amount 到账金额，单位元
     * @param method 支付渠道：WECHAT、ALIPAY、WALLET
     * @param transaction 渠道交易流水号
     * @throws ServiceException 金额或渠道不匹配、客户不存在
     */
    void paid(String sn, Double amount, String method, String transaction);

    /**
     * 分页查询会员购买记录；累计节省金额由会员中心按客户统计，不分摊到开通或续费订单。
     *
     * @param memberId 客户 ID，空值时不限制客户，由调用方控制查询权限
     * @param keyword 客户名称模糊查询条件
     * @param status 状态筛选，空值时不筛选
     * @param page 分页参数
     * @return 按创建时间及 ID 倒序排列的购买记录
     */
    IPage<PaidMemberOrder> page(String memberId, String keyword, String status, PageVO page);

    /**
     * 按配置检查实物商品商家范围，获取统一或逐 SKU 折扣。
     * 此处不校验会员资格、活动互斥及上下架状态，由调用方负责。
     *
     * @param config 当前配置或有效订单的成交快照
     * @param sku 待计算商品 SKU
     * @return 折扣，单位折；商家、类型或 SKU 不适用时返回 null
     */
    BigDecimal discount(PaidMemberSetting config, GoodsSku sku);

    /**
     * 批量生成会员价：有效会员使用成交快照，未开通客户使用当前配置预览。
     * 过滤不适用、无实际折扣、活动专用、下架或未审核商品；图片标签始终使用当前展示配置。
     *
     * @param memberId 客户 ID
     * @param skuIds SKU 主键列表，最多 100 个
     * @return 以 SKU ID 为键的报价，含元单位价格、折扣、资格及标签
     * @throws ServiceException 列表为空值或超出上限
     */
    Map<String, Object> quotes(String memberId, List<String> skuIds);

    /**
     * 按主键游标扫描当前有效的已支付订单，供周期权益任务处理。
     *
     * @param cursor 上批最后一个 ID，空值从头开始
     * @return ID 升序的有效订单，每批最多 200 条
     */
    List<PaidMemberOrder> activeOrders(String cursor);

    /**
     * 按主键定位权益等内部业务的来源会员订单。
     *
     * @param id li_paid_member_order 主键
     * @return 订单，不存在时返回 null
     */
    PaidMemberOrder findById(String id);

    /**
     * 判断订单是否已支付且当前处于左闭右开的有效区间。
     *
     * @param order 待判断订单，可为 null
     * @return 是否拥有当前有效资格
     */
    boolean isActive(PaidMemberOrder order);

    /**
     * 检查权益来源订单的有效期、客户归属、快照关联及权益当前开启状态。
     *
     * @param memberId 领取客户 ID
     * @param orderId 来源会员订单主键
     * @param benefitId 待发放权益主键
     * @return 是否允许使用该来源订单发放此权益
     */
    boolean isBenefitEligible(String memberId, String orderId, String benefitId);
}
