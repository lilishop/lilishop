package cn.lili.controller.manager.distribution;

import cn.hutool.json.JSONUtil;
import cn.lili.common.aop.annotation.DemoSite;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.ResultUtil;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.vo.ResultMessage;
import cn.lili.modules.distribution.entity.enums.DistributionBindModeEnum;
import cn.lili.modules.distribution.entity.enums.DistributionPermanentBindModeEnum;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.distribution.entity.dto.DistributionFeaturedGoodsSettingSaveDTO;
import cn.lili.modules.distribution.entity.vos.DistributionFeaturedGoodsSettingVO;
import cn.lili.modules.distribution.service.DistributionFeaturedGoodsService;
import cn.lili.modules.distribution.support.DistributionInviteCardSettingSupport;
import cn.lili.modules.distribution.support.DistributionPromotionPosterSettingSupport;
import cn.lili.modules.distribution.service.DistributionRecruitService;
import cn.lili.modules.system.entity.dto.DistributionCustomerSetting;
import cn.lili.modules.system.entity.dto.DistributionInviteCardSetting;
import cn.lili.modules.system.entity.dto.DistributionPromotionPosterSetting;
import cn.lili.modules.system.entity.dto.DistributionRecruitSetting;
import cn.lili.modules.system.entity.dto.DistributionSettlementSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;

/**
 * 管理端,分销V2设置接口
 *
 * @author Bulbasaur
 * @since 2026-07-28
 */
@RestController
@Tag(name = "管理端,分销V2设置接口")
@RequestMapping("/manager/distribution/setting/v2")
public class DistributionSettingV2ManagerController {

    @Autowired
    private SettingService settingService;

    @Autowired
    private DistributionFeaturedGoodsService distributionFeaturedGoodsService;

    @Autowired
    private DistributionPromotionPosterSettingSupport distributionPromotionPosterSettingSupport;

    @Autowired
    private DistributionInviteCardSettingSupport distributionInviteCardSettingSupport;

    @Autowired
    private DistributionRecruitService distributionRecruitService;

    @Operation(summary = "获取分销招募设置")
    @GetMapping("/recruit")
    public ResultMessage<DistributionRecruitSetting> getRecruit() {
        return ResultUtil.data(distributionRecruitService.getSetting());
    }

    @DemoSite
    @Operation(summary = "更新分销招募设置")
    @PutMapping("/recruit")
    public ResultMessage<Object> putRecruit(@RequestBody DistributionRecruitSetting setting) {
        distributionRecruitService.saveSetting(setting);
        return ResultUtil.success();
    }

    @Operation(summary = "获取客户关系设置")
    @GetMapping("/customer")
    public ResultMessage<DistributionCustomerSetting> getCustomer() {
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_CUSTOMER_SETTING.name());
        if (setting == null || setting.getSettingValue() == null) {
            return ResultUtil.data(null);
        }
        return ResultUtil.data(JSONUtil.toBean(setting.getSettingValue(), DistributionCustomerSetting.class));
    }

    @DemoSite
    @Operation(summary = "更新客户关系设置")
    @PutMapping("/customer")
    public ResultMessage<Object> putCustomer(@RequestBody DistributionCustomerSetting setting) {
        validateCustomerSetting(setting);
        saveSetting(SettingEnum.DISTRIBUTION_CUSTOMER_SETTING, setting);
        return ResultUtil.success();
    }

    @Operation(summary = "获取结算方案设置")
    @GetMapping("/settlement")
    public ResultMessage<DistributionSettlementSetting> getSettlement() {
        return ResultUtil.data(readSetting(SettingEnum.DISTRIBUTION_SETTLEMENT_SETTING, DistributionSettlementSetting.class, new DistributionSettlementSetting()));
    }

    @DemoSite
    @Operation(summary = "更新结算方案设置")
    @PutMapping("/settlement")
    public ResultMessage<Object> putSettlement(@RequestBody DistributionSettlementSetting setting) {
        saveSetting(SettingEnum.DISTRIBUTION_SETTLEMENT_SETTING, setting);
        return ResultUtil.success();
    }

    @Operation(summary = "获取分销商品设置")
    @GetMapping("/featured-goods")
    public ResultMessage<DistributionFeaturedGoodsSettingVO> getFeaturedGoods() {
        return ResultUtil.data(distributionFeaturedGoodsService.getSettingDetail());
    }

    @DemoSite
    @Operation(summary = "更新分销商品设置")
    @PutMapping("/featured-goods")
    public ResultMessage<Object> putFeaturedGoods(@RequestBody DistributionFeaturedGoodsSettingSaveDTO setting) {
        distributionFeaturedGoodsService.saveSetting(setting);
        return ResultUtil.success();
    }

    @Operation(summary = "获取平台海报设置")
    @GetMapping("/promotion-poster")
    public ResultMessage<DistributionPromotionPosterSetting> getPromotionPoster() {
        return ResultUtil.data(distributionPromotionPosterSettingSupport.readSetting());
    }

    @DemoSite
    @Operation(summary = "更新平台海报设置")
    @PutMapping("/promotion-poster")
    public ResultMessage<Object> putPromotionPoster(@RequestBody DistributionPromotionPosterSetting setting) {
        distributionPromotionPosterSettingSupport.saveSetting(setting);
        return ResultUtil.success();
    }

    @Operation(summary = "获取邀请卡设置")
    @GetMapping("/invite-card")
    public ResultMessage<DistributionInviteCardSetting> getInviteCard() {
        return ResultUtil.data(distributionInviteCardSettingSupport.readSetting());
    }

    @DemoSite
    @Operation(summary = "更新邀请卡设置")
    @PutMapping("/invite-card")
    public ResultMessage<Object> putInviteCard(@RequestBody DistributionInviteCardSetting setting) {
        distributionInviteCardSettingSupport.saveSetting(setting);
        return ResultUtil.success();
    }

    /**
     * 从系统配置表读取并反序列化分销设置。
     *
     * @param settingEnum  配置项枚举
     * @param clazz        目标类型
     * @param defaultValue 配置不存在时的默认值
     * @param <T>          设置类型
     * @return 反序列化后的配置对象
     */
    private <T> T readSetting(SettingEnum settingEnum, Class<T> clazz, T defaultValue) {
        Setting setting = settingService.get(settingEnum.name());
        if (setting == null || setting.getSettingValue() == null) {
            return defaultValue;
        }
        return JSONUtil.toBean(setting.getSettingValue(), clazz);
    }

    /**
     * 将分销设置序列化后写入系统配置表（不存在则新建）。
     *
     * @param settingEnum 配置项枚举
     * @param value       待保存的设置对象
     */
    private void saveSetting(SettingEnum settingEnum, Object value) {
        Setting setting = settingService.getById(settingEnum.name());
        if (setting == null) {
            setting = new Setting();
            setting.setId(settingEnum.name());
        }
        setting.setSettingValue(JSONUtil.toJsonStr(value));
        settingService.saveUpdate(setting);
    }

    /**
     * 校验客户关系设置的业务规则。
     *
     * @param setting 客户关系设置
     */
    private void validateCustomerSetting(DistributionCustomerSetting setting) {
        if (DistributionBindModeEnum.PERMANENT.name().equals(setting.getBindMode())
                && DistributionPermanentBindModeEnum.PAID_WITHIN_DAYS.name().equals(setting.getPermanentBindMode())
                && (setting.getPermanentPaidDays() == null || setting.getPermanentPaidDays() < 1)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "永久锁客支付天数不能低于1天");
        }
    }
}
