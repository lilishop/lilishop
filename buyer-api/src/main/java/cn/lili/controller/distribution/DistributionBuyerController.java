package cn.lili.controller.distribution;

import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.ResultMessage;
import cn.lili.common.security.context.UserContext;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dos.DistributionRecruitApplication;
import cn.lili.modules.distribution.entity.dto.DistributionRecruitApplyDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerGradeListVO;
import cn.lili.modules.distribution.entity.vos.DistributionParentInfoVO;
import cn.lili.modules.distribution.entity.vos.DistributionInviteCardVO;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerCustomerQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionBuyerPerformanceQueryDTO;
import cn.lili.modules.distribution.entity.dto.DistributionInviteeQueryDTO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerDetailVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerCustomerPageVO;
import cn.lili.modules.distribution.entity.vos.DistributionBuyerPerformanceVO;
import cn.lili.modules.distribution.entity.vos.DistributionInvitePageVO;
import cn.lili.modules.distribution.entity.vos.DistributionPromotionPosterVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitConditionProgressVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitInviterVO;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitPageVO;
import cn.lili.modules.distribution.service.DistributionBuyerPerformanceService;
import cn.lili.modules.distribution.service.DistributionCustomerRelationService;
import cn.lili.modules.distribution.service.DistributionGoodsService;
import cn.lili.modules.distribution.service.DistributionGradeService;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.distribution.support.DistributionBuyerViewSupport;
import cn.lili.modules.distribution.support.DistributionSettingSupport;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


/**
 * 买家端,分销员接口
 *
 * @author pikachu
 * @since 2020/11/16 10:03 下午
 */
@RestController
@Tag(name = "买家端,分销员接口")
@RequestMapping("/buyer/distribution/distribution")
public class DistributionBuyerController {

    private static final Logger log = LoggerFactory.getLogger(DistributionBuyerController.class);

    @Autowired
    private DistributionService distributionService;

    @Autowired
    private DistributionRecruitService recruitService;

    @Autowired
    private DistributionBuyerPerformanceService buyerPerformanceService;

    @Autowired
    private DistributionCustomerRelationService customerRelationService;

    @Autowired
    private DistributionGradeService gradeService;

    @Autowired
    private DistributionGoodsService distributionGoodsService;

    @Autowired
    private DistributionSettingSupport distributionSettingSupport;

    @Operation(summary = "获取当前客户的分销员信息", description = "可根据分销员信息查询待提现金额以及冻结金额等信息")
    @GetMapping
    public ResultMessage<Distribution> getDistribution() {
        distributionService.checkDistributionSetting();
        // 清退必须回对象供入口按状态路由；金额脱敏在拷贝上完成，不写库
        return ResultUtil.data(DistributionBuyerViewSupport.forBuyerResponse(distributionService.getDistribution()));
    }

    @Operation(summary = "推广海报上下文（邀请好友锁客）")
    @GetMapping("/promotion-poster")
    public ResultMessage<DistributionPromotionPosterVO> promotionPoster() {
        return ResultUtil.data(distributionService.getPromotionPoster());
    }

    @Operation(summary = "邀请卡上下文（招募下级分销员）")
    @GetMapping("/invite-card")
    public ResultMessage<DistributionInviteCardVO> inviteCard() {
        return ResultUtil.data(distributionService.getInviteCard());
    }

    @Operation(summary = "业绩统计")
    @GetMapping("/performance")
    public ResultMessage<DistributionBuyerPerformanceVO> performance(DistributionBuyerPerformanceQueryDTO query) {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return ResultUtil.data(new DistributionBuyerPerformanceVO());
        }
        return ResultUtil.data(buyerPerformanceService.getPerformance(distribution.getId(), query));
    }

    @Operation(summary = "我邀请的下级分销员列表")
    @GetMapping("/invite/invitees")
    public ResultMessage<DistributionInvitePageVO> invitees(DistributionInviteeQueryDTO query) {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return ResultUtil.data(new DistributionInvitePageVO());
        }
        return ResultUtil.data(distributionService.pageInvitees(distribution.getId(), query));
    }

    @Operation(summary = "我的客户列表")
    @GetMapping("/customers")
    public ResultMessage<DistributionBuyerCustomerPageVO> customers(DistributionBuyerCustomerQueryDTO query) {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return ResultUtil.data(new DistributionBuyerCustomerPageVO());
        }
        return ResultUtil.data(customerRelationService.pageBuyerCustomers(distribution.getId(), query));
    }

    @Operation(summary = "客户详情")
    @GetMapping("/customers/{id}")
    public ResultMessage<DistributionBuyerCustomerDetailVO> customerDetail(@PathVariable String id) {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return ResultUtil.error(ResultCode.ERROR);
        }
        return ResultUtil.data(customerRelationService.getBuyerCustomerDetail(distribution.getId(), id));
    }

    @Operation(summary = "设置/取消客户星标")
    @PutMapping("/customers/{id}/starred")
    public ResultMessage<Object> updateCustomerStarred(@PathVariable String id,
                                                       @RequestParam Boolean starred) {
        distributionService.checkDistributionSetting();
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null || distributionService.isRetreated(distribution.getMemberId())) {
            return ResultUtil.error(ResultCode.ERROR);
        }
        customerRelationService.updateBuyerCustomerStarred(distribution.getId(), id, starred);
        return ResultUtil.success();
    }

    @Operation(summary = "绑定分销员")
    @Parameter(name = "distributionId", description = "分销员ID", required = true)
    @GetMapping("/bindingDistribution/{distributionId}")
    public ResultMessage<Object> bindingDistribution(@PathVariable String distributionId) {
        distributionService.bindingDistribution(distributionId);
        return ResultUtil.success();
    }

    @Operation(summary = "分享商品访问（记录访问并绑定客户关系）")
    @GetMapping("/share-visit")
    public ResultMessage<Object> shareVisit(
            @RequestParam String skuId,
            @RequestParam String goodsId,
            @RequestParam String distributionId,
            @RequestParam(required = false) String shareId) {
        distributionGoodsService.recordShareVisit(skuId, goodsId, distributionId, shareId);
        if (UserContext.getCurrentUser() != null) {
            try {
                distributionService.bindingDistribution(distributionId);
            } catch (Exception ignored) {
                // 访问记录优先，绑定失败不影响页面访问
            }
        }
        return ResultUtil.success();
    }

    @Operation(summary = "招募页信息")
    @GetMapping("/recruit/page")
    public ResultMessage<DistributionRecruitPageVO> recruitPage() {
        tryAutoApplyQuietly();
        return ResultUtil.data(recruitService.getRecruitPage());
    }

    @Operation(summary = "招募页邀请人信息（无需登录）")
    @GetMapping("/recruit/inviter")
    public ResultMessage<DistributionRecruitInviterVO> recruitInviter(@RequestParam String distributionId) {
        return ResultUtil.data(recruitService.getRecruitInviter(distributionId));
    }

    @Operation(summary = "提交招募申请")
    @PostMapping("/recruit/apply")
    public ResultMessage<DistributionRecruitApplication> recruitApply(@RequestBody DistributionRecruitApplyDTO applyDTO) {
        return ResultUtil.data(recruitService.submitApplication(applyDTO));
    }

    @Operation(summary = "上报招募邀请 token（登录后供自动申请使用）")
    @PutMapping("/recruit/pending-token")
    public ResultMessage<Object> savePendingRecruitToken(@RequestParam String recruitToken) {
        String memberId = UserContext.getCurrentUser().getId();
        recruitService.savePendingRecruitToken(memberId, recruitToken);
        return ResultUtil.success();
    }

    @Operation(summary = "招募条件进度")
    @GetMapping("/recruit/progress")
    public ResultMessage<DistributionRecruitConditionProgressVO> recruitProgress() {
        tryAutoApplyQuietly();
        String memberId = UserContext.getCurrentUser() == null ? null : UserContext.getCurrentUser().getId();
        return ResultUtil.data(recruitService.getConditionProgress(memberId));
    }

    /**
     * 已登录时尝试自动申请；失败不影响招募页读取。
     */
    private void tryAutoApplyQuietly() {
        if (UserContext.getCurrentUser() == null) {
            return;
        }
        try {
            recruitService.tryAutoApply(UserContext.getCurrentUser().getId());
        } catch (Exception e) {
            log.error("招募页自动申请失败", e);
        }
    }

    @Operation(summary = "上级分销员信息（附带是否展示上级）")
    @GetMapping("/parent")
    public ResultMessage<DistributionParentInfoVO> parentInfo() {
        DistributionParentInfoVO vo = new DistributionParentInfoVO();
        DistributionSetting setting = distributionSettingSupport.getSetting();
        vo.setParentDistributorVisible(setting.getParentDistributorVisible());
        Distribution distribution = distributionService.getDistribution();
        if (distribution == null) {
            return ResultUtil.data(vo);
        }
        if (StringUtils.isNotEmpty(distribution.getParentDistributionId())) {
            vo.setParentDistributionId(distribution.getParentDistributionId());
            Distribution parent = distributionService.getById(distribution.getParentDistributionId());
            if (parent != null) {
                vo.setParentNickname(parent.getMemberName());
            }
        }
        return ResultUtil.data(vo);
    }

    @Operation(summary = "等级列表（附带是否展示各等级规则与权益）")
    @GetMapping("/grades")
    public ResultMessage<DistributionBuyerGradeListVO> gradeList() {
        DistributionSetting setting = distributionSettingSupport.getSetting();
        DistributionBuyerGradeListVO vo = new DistributionBuyerGradeListVO();
        vo.setUpgradeConditionVisible(setting.getUpgradeConditionVisible());
        vo.setGrades(gradeService.listGrades());
        return ResultUtil.data(vo);
    }
}
