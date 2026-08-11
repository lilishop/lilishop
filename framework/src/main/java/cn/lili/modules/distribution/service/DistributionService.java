package cn.lili.modules.distribution.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionMemberBatchGradeDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberCreateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionMemberUpdateDTO;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.entity.vos.DistributionMemberDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionMemberVO;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionInviteCardVO;
import cn.lili.modules.distribution.entity.vos.DistributionInvitePageVO;
import cn.lili.modules.distribution.entity.vos.DistributionPromotionPosterVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;
import jakarta.servlet.http.HttpServletResponse;

/**
 * 分销员业务层。
 * <p>
 * 负责分销员申请、审核、清退/恢复、佣金账户变动及管理端列表查询。
 * </p>
 *
 * @author pikachu
 * @since 2020-03-14
 */
public interface DistributionService extends IService<Distribution> {

    /**
     * 分页查询分销员（基础实体）。
     *
     * @param distributionSearchParams 搜索条件
     * @param page                     分页参数
     * @return 分销员分页
     */
    IPage<Distribution> distributionPage(DistributionSearchParams distributionSearchParams, PageVO page);

    /**
     * 获取当前登录会员的分销员信息。
     *
     * @return 分销员信息，未申请时返回 null
     */
    Distribution getDistribution();

    /**
     * 平台审核分销员申请。
     *
     * @param id     分销员 ID
     * @param status 审核状态（PASS/REFUSE）
     * @return 是否操作成功
     */
    boolean audit(String id, String status);

    /**
     * 清退分销员。
     *
     * @param id 分销员 ID
     * @return 是否操作成功
     */
    boolean retreat(String id);

    /**
     * 恢复已清退的分销员。
     *
     * @param id 分销员 ID
     * @return 是否操作成功
     */
    boolean resume(String id);

    /**
     * 会员绑定上级分销员关系。
     *
     * @param distributionId 上级分销员 ID
     */
    void bindingDistribution(String distributionId);

    /**
     * 校验分销功能是否已开启。
     */
    void checkDistributionSetting();

    /**
     * 增加分销冻结佣金（下单支付时调用）。
     *
     * @param rebate                 佣金金额
     * @param distributionId         分销员 ID
     * @param distributionOrderPrice 分销订单金额
     */
    void addRebate(Double rebate, String distributionId, Double distributionOrderPrice);

    /**
     * 扣减分销冻结佣金（取消/退款时调用）。
     *
     * @param rebate                 佣金金额
     * @param distributionId         分销员 ID
     * @param distributionOrderPrice 分销订单金额
     */
    void subRebate(Double rebate, String distributionId, Double distributionOrderPrice);

    /**
     * 将冻结佣金转为可提现金额（订单完成时调用）。
     *
     * @param rebate         佣金金额
     * @param distributionId 分销员 ID
     */
    void addCanRebate(Double rebate, String distributionId);

    /**
     * 扣减可提现金额（提现申请时调用）。
     *
     * @param rebate         金额
     * @param distributionId 分销员 ID
     */
    void addCashRebate(Double rebate, String distributionId);

    /**
     * 回退提现扣款（提现拒绝时调用）。
     *
     * @param rebate         金额
     * @param distributionId 分销员 ID
     */
    void subCashRebate(Double rebate, String distributionId);

    /**
     * 判断会员对应的分销员是否已被清退。
     *
     * @param memberId 会员 ID
     * @return 是否已清退
     */
    boolean isRetreated(String memberId);

    /**
     * 管理端分页查询分销员（V2 会员视图，含等级、邀请方、业绩等指标）。
     *
     * @param distributionSearchParams 搜索条件
     * @param page                     分页参数
     * @return 分销员会员视图分页
     */
    IPage<DistributionMemberVO> getMemberPage(DistributionSearchParams distributionSearchParams, PageVO page);

    /**
     * 管理端查询分销员详情。
     *
     * @param id 分销员 ID
     * @return 分销员详情
     */
    DistributionMemberDetailVO getMemberDetail(String id);

    /**
     * 管理端编辑分销员基础信息。
     *
     * @param id        分销员 ID
     * @param updateDTO 编辑参数
     * @return 更新后的分销员信息
     */
    DistributionMemberVO updateMember(String id, DistributionMemberUpdateDTO updateDTO);

    /**
     * 管理端新增分销员（同步创建或关联 C 端客户）。
     *
     * @param createDTO 新增参数
     * @return 新建的分销员信息
     */
    DistributionMemberVO createMember(DistributionMemberCreateDTO createDTO);

    /**
     * 管理端批量设置分销等级。
     *
     * @param batchGradeDTO 批量等级参数
     */
    void batchUpdateGrade(DistributionMemberBatchGradeDTO batchGradeDTO);

    /**
     * 管理端按筛选条件导出分销员列表。
     *
     * @param response                 HTTP 响应
     * @param distributionSearchParams 搜索条件
     */
    void exportMembers(HttpServletResponse response, DistributionSearchParams distributionSearchParams);

    /**
     * 获取推广海报上下文（邀请好友锁客，非招募下级）。
     *
     * @return 推广海报上下文
     */
    DistributionPromotionPosterVO getPromotionPoster();

    /**
     * 获取邀请卡上下文（招募下级分销员）。
     *
     * @return 邀请卡上下文
     */
    DistributionInviteCardVO getInviteCard();

    /**
     * 招募审核通过后绑定上级分销员。
     *
     * @param childDistributionId 下级分销员ID
     * @param recruitInviterId    招募邀请方分销员ID
     * @param boundDistributorId  已绑定上级分销员ID
     */
    void bindParentOnAuditPass(String childDistributionId, String recruitInviterId, String boundDistributorId);

    /**
     * 管理端修改下级分销员的邀请上级。
     *
     * @param childDistributionId  下级分销员ID
     * @param parentDistributionId 上级分销员ID，null 表示清除
     */
    void updateParentByAdmin(String childDistributionId, String parentDistributionId);

    /**
     * 分页查询下级邀请分销员列表。
     *
     * @param parentDistributionId 上级分销员ID
     * @param query                查询条件
     * @return 邀请分页结果
     */
    DistributionInvitePageVO pageInvitees(String parentDistributionId, DistributionInviteeQueryDTO query);
}
