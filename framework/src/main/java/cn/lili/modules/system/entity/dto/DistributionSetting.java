package cn.lili.modules.system.entity.dto;

import cn.lili.modules.distribution.entity.enums.DistributionParentAttributionModeEnum;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.io.Serializable;

/**
 * 分销配置
 *
 * @author Chopper
 * @since 2020/12/23 18:35
 */
@Data
public class DistributionSetting implements Serializable {

    private static final long serialVersionUID = 2099524659914361438L;

    public static final String DEFAULT_DISTRIBUTOR_NAME = "分销员";

    /**
     * 是否开启分销
     */
    @Schema(description = "是否开启分销")
    private Boolean isOpen;

    /**
     * 分销员对外展示名称
     */
    @Schema(description = "分销员名称")
    private String distributorName;

    @Schema(description = "是否展示等级升级条件及权益")
    private Boolean upgradeConditionVisible;

    @Schema(description = "是否展示上级分销员信息")
    private Boolean parentDistributorVisible;

    /**
     * @see DistributionParentAttributionModeEnum
     */
    @Schema(description = "分销员上级归属模式")
    private String parentAttributionMode;

    public static DistributionSetting defaultSetting() {
        DistributionSetting setting = new DistributionSetting();
        setting.setIsOpen(true);
        setting.setDistributorName(DEFAULT_DISTRIBUTOR_NAME);
        setting.setUpgradeConditionVisible(true);
        setting.setParentDistributorVisible(true);
        setting.setParentAttributionMode(DistributionParentAttributionModeEnum.RECRUIT_INVITER.name());
        return setting;
    }

    public void applyDefaults() {
        if (isOpen == null) {
            isOpen = true;
        }
        if (distributorName == null || distributorName.isBlank()) {
            distributorName = DEFAULT_DISTRIBUTOR_NAME;
        } else {
            distributorName = distributorName.trim();
        }
        if (upgradeConditionVisible == null) {
            upgradeConditionVisible = true;
        }
        if (parentDistributorVisible == null) {
            parentDistributorVisible = true;
        }
        if (parentAttributionMode == null || parentAttributionMode.isBlank()) {
            parentAttributionMode = DistributionParentAttributionModeEnum.RECRUIT_INVITER.name();
        }
    }

    public String resolveDistributorName() {
        applyDefaults();
        return distributorName;
    }
}
