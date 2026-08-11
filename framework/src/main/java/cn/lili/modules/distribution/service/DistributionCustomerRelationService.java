package cn.lili.modules.distribution.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.DistributionCustomerRelation;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerCustomerQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionCustomerRelationSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationLogVO;
import cn.lili.modules.distribution.entity.vos.DistributionCustomerRelationVO;
import cn.lili.modules.system.entity.dto.DistributionCustomerSetting;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 分销客户关系服务。
 * <p>
 * 管理会员与分销员之间的绑定关系，包括扫码绑定、支付转永久、
 * 后台改绑/解绑及关系到期处理。
 * </p>
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
public interface DistributionCustomerRelationService extends IService<DistributionCustomerRelation> {

    /**
     * 获取分销客户绑定相关系统配置。
     *
     * @return 客户绑定设置
     */
    DistributionCustomerSetting getCustomerSetting();

    /**
     * 会员扫码后建立与分销员的客户关系。
     *
     * @param distributionId 分销员 ID
     * @param memberId       会员 ID
     * @param source         绑定来源
     */
    void bindOnScan(String distributionId, String memberId, String source);

    /**
     * 会员支付成功后，将短期关系转为永久关系。
     *
     * @param memberId 会员 ID
     * @param orderSn  订单编号
     */
    void bindPermanentOnPaid(String memberId, String orderSn);

    /**
     * 查询会员当前有效的分销客户关系。
     *
     * @param memberId 会员 ID
     * @return 有效关系，不存在时返回 null
     */
    DistributionCustomerRelation getActiveRelation(String memberId);

    /**
     * 管理端将会员改绑到指定分销员。
     *
     * @param memberId       会员 ID
     * @param distributionId 目标分销员 ID
     * @param reason         改绑原因
     * @param operatorId     操作人 ID
     */
    void adminRebind(String memberId, String distributionId, String reason, String operatorId);

    /**
     * 管理端解除会员与分销员的绑定关系。
     *
     * @param memberId   会员 ID
     * @param reason     解绑原因
     * @param operatorId 操作人 ID
     */
    void adminUnbind(String memberId, String reason, String operatorId);

    /**
     * 批量处理已到期的待转永久客户关系。
     *
     * @return 处理条数
     */
    int expirePendingRelations();

    /**
     * 管理端分页查询分销客户关系。
     *
     * @param params 搜索条件
     * @param page   分页参数
     * @return 客户关系分页
     */
    IPage<DistributionCustomerRelationVO> relationPage(DistributionCustomerRelationSearchParams params, PageVO page);

    /**
     * 管理端查询客户关系详情。
     *
     * @param id 关系 ID
     * @return 详情
     */
    DistributionCustomerRelationDetailVO getRelationDetail(String id);

    /**
     * 管理端查询客户关系变更记录。
     *
     * @param relationId 关系 ID
     * @return 变更记录列表
     */
    List<DistributionCustomerRelationLogVO> listRelationLogs(String relationId);

    /**
     * 分销员端分页查询自己的客户列表。
     *
     * @param distributionId 分销员 ID
     * @param query          查询条件
     * @return 客户分页
     */
    DistributionBuyerCustomerPageVO pageBuyerCustomers(String distributionId, DistributionBuyerCustomerQueryDTO query);

    /**
     * 分销员端设置/取消客户星标。
     *
     * @param distributionId 分销员 ID
     * @param relationId     关系 ID
     * @param starred        是否星标
     */
    void updateBuyerCustomerStarred(String distributionId, String relationId, Boolean starred);

    /**
     * 分销员端查询客户详情（含成交统计）。
     *
     * @param distributionId 分销员 ID
     * @param relationId     关系 ID
     * @return 客户详情
     */
    DistributionBuyerCustomerDetailVO getBuyerCustomerDetail(String distributionId, String relationId);
}
