package cn.lili.modules.system.entity.dto;

import cn.lili.modules.distribution.entity.enums.RecruitApplyModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitAuditModeEnum;
import cn.lili.modules.distribution.entity.enums.RecruitJoinConditionTypeEnum;
import cn.lili.modules.distribution.entity.vos.DistributionRecruitPlanGoodsItemVO;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * 分销招募设置
 *
 * @author pikachu
 */
@Data
public class DistributionRecruitSetting implements Serializable {

    private static final long serialVersionUID = 1L;

    @Schema(description = "设置版本号（每次保存递增，申请记录快照用）")
    private Integer planVersion;

    /**
     * @see RecruitJoinConditionTypeEnum
     */
    @Schema(description = "加入条件类型")
    private String joinConditionType;

    /**
     * @see RecruitApplyModeEnum
     */
    @Schema(description = "申请方式")
    private String applyMode;

    /**
     * @see RecruitAuditModeEnum
     */
    @Schema(description = "审核方式")
    private String auditMode;

    @Schema(description = "是否展示线上招募入口")
    private Boolean onlineEntryVisible;

    @Schema(description = "是否需要填写申请信息")
    private Boolean requireApplyInfo;

    @Schema(description = "分销推广服务协议（富文本）")
    private String agreementContent;

    @Schema(description = "是否启用需购买指定商品")
    private Boolean requirePurchaseGoods;

    @Schema(description = "是否启用自购金额门槛")
    private Boolean requireSelfPurchaseAmount;

    @Schema(description = "是否启用消费笔数门槛")
    private Boolean requireConsumeCount;

    @Schema(description = "自购金额门槛")
    private BigDecimal minSelfPurchaseAmount;

    @Schema(description = "消费笔数门槛")
    private Integer minConsumeCount;

    @Schema(description = "指定商品展示列表")
    private List<DistributionRecruitPlanGoodsItemVO> recruitGoods;

    @Schema(description = "是否启用招募海报")
    private Boolean posterEnabled;

    @Schema(description = "海报模板")
    private String posterTemplate;

    @Schema(description = "海报背景图")
    private String posterBackground;

    @Schema(description = "海报标题")
    private String posterTitle;

    @Schema(description = "海报权益文案")
    private String posterBenefitText;

    @Schema(description = "海报规则文案")
    private String posterRuleText;

    @Schema(description = "海报联系方式")
    private String posterContact;

    @Schema(description = "海报二维码位置配置(JSON)")
    private String posterQrcodePosition;

    @Schema(description = "申请动态字段配置")
    private List<DistributionRecruitFieldSetting> fields;

    public static DistributionRecruitSetting defaultSetting() {
        DistributionRecruitSetting setting = new DistributionRecruitSetting();
        setting.setPlanVersion(0);
        setting.setJoinConditionType(RecruitJoinConditionTypeEnum.NONE.name());
        setting.setApplyMode(RecruitApplyModeEnum.MANUAL.name());
        setting.setAuditMode(RecruitAuditModeEnum.MANUAL.name());
        setting.setOnlineEntryVisible(true);
        setting.setRequireApplyInfo(false);
        setting.setRequirePurchaseGoods(false);
        setting.setRequireSelfPurchaseAmount(false);
        setting.setRequireConsumeCount(false);
        setting.setMinSelfPurchaseAmount(new BigDecimal("0.01"));
        setting.setMinConsumeCount(1);
        setting.setRecruitGoods(new ArrayList<>());
        setting.setPosterEnabled(false);
        setting.setFields(new ArrayList<>());
        return setting;
    }
}
