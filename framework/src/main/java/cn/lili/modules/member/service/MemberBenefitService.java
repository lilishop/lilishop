package cn.lili.modules.member.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.vo.EnumValueVO;
import cn.lili.modules.member.entity.vo.MemberBenefitDetailVO;
import cn.lili.modules.member.entity.vo.MemberBenefitGrantRecordVO;
import cn.lili.modules.member.entity.vo.MemberCurrentBenefitVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 客户权益管理与发放服务，负责权益配置、等级权益及成长进度查询、积分倍率计算、周期发放和失败补发。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public interface MemberBenefitService extends IService<MemberBenefit> {

    /**
     * 新增客户权益（含业务校验）
     *
     * @param memberBenefit 客户权益
     */
    void saveBenefit(MemberBenefit memberBenefit);

    /**
     * 更新客户权益（含业务校验）
     *
     * @param id 权益ID
     * @param memberBenefit 客户权益
     */
    void updateBenefit(String id, MemberBenefit memberBenefit);

    /**
     * 切换客户权益状态
     *
     * @param id 权益ID
     * @param state 状态 OPEN/CLOSE
     */
    void updateBenefitState(String id, String state);

    /**
     * 删除客户权益（若被等级引用则拒绝删除）
     *
     * @param id 权益ID
     */
    void deleteBenefit(String id);

    /**
     * 客户权益分页查询
     *
     * @param keyword 关键字（权益名称，模糊匹配）
     * @param state 状态 OPEN/CLOSE（可选）
     * @param page 分页参数
     * @return 分页结果
     */
    IPage<MemberBenefit> getBenefitPage(String keyword, String state, PageVO page);

    /**
     * 客户权益列表查询
     *
     * @param onlyOpen 是否仅返回启用权益
     * @return 权益列表
     */
    List<MemberBenefit> listBenefits(Boolean onlyOpen);

    /**
     * 客户权益类型枚举列表（用于下拉选择）
     *
     * @return 类型列表（value/description）
     */
    List<EnumValueVO> listBenefitTypes();

    /**
     * 按ID列表查询权益，按权益排序值返回，不保证与入参顺序一致。
     *
     * @param ids 权益ID列表
     * @param onlyOpen 是否仅返回启用权益
     * @return 权益列表
     */
    List<MemberBenefit> listByIdList(List<String> ids, Boolean onlyOpen);

    /**
     * 获取当前客户的有效普通等级、已开启权益及等级升级经验进度。
     *
     * @param memberId 会员ID
     * @return 等级与权益，以及当前经验、下一等级、升级所差经验和进度
     */
    MemberCurrentBenefitVO getCurrentMemberBenefits(String memberId);

    /**
     * 管理端权益详情（券礼包类型联查优惠券名称、面值、有效期等）
     *
     * @param id 权益ID
     * @return 详情
     */
    MemberBenefitDetailVO getManagerBenefitDetail(String id);

    /**
     * 客户达到等级后尝试发放该等级的自动权益，有活动事务时等待提交后执行。
     * 一次性权益按客户、等级、权益去重；周期券按发放周期去重，后续由周期任务继续处理。
     *
     * @param memberId 会员ID
     * @param gradeId  达到的等级ID
     */
    void tryGrantBenefitsForGradeReached(String memberId, String gradeId);

    /**
     * 管理端分页查询等级权益初始化记录，不代表各项权益均已发放成功。
     * 逐项发放状态及失败原因以权益发放明细为准。
     *
     * @param mobile  会员手机号（精确匹配，可选）
     * @param gradeId 等级ID（可选）
     * @param page    分页
     * @return 分页数据
     */
    IPage<MemberBenefitGrantRecordVO> pageBenefitGrantRecords(String mobile, String gradeId, PageVO page);

    /**
     * 获取普通等级与当前有效付费会员权益中的最高积分倍率，同类权益不叠乘。
     *
     * @param memberId 客户ID
     * @return 最高积分倍率，无倍率权益时为1
     */
    java.math.BigDecimal getPointMultiplier(String memberId);

    /**
     * 扫描有效付费会员订单及客户当前等级，生成并发放当前应享的权益。
     * 恢复待发放记录，并自动重试仍有领取资格且尝试次数小于10的失败记录；不追补未生成的历史周期。
     */
    void processScheduledBenefits();

    /**
     * 手动补发失败的逐项权益记录，发放前重新检查等级或付费订单资格及权益开启状态。
     *
     * @param id li_member_benefit_delivery 发放明细主键
     * @throws cn.lili.common.exception.ServiceException 记录不存在、状态非FAILED或客户不再享有该权益
     */
    void retryBenefitGrant(String id);

    /**
     * 按付费会员订单的配置快照发放仍处于开启状态的权益，订单须已支付且处于有效区间。
     * 未来生效的续费区间暂不发放，一次性及周期权益由发放账本分别去重。
     *
     * @param orderId li_paid_member_order 订单主键，不是业务订单号
     */
    void deliverPaidMemberBenefits(String orderId);
}
