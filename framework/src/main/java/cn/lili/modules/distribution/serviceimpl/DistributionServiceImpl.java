package cn.lili.modules.distribution.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.BeanUtil;
import cn.lili.mybatis.util.PageUtil;
import cn.lili.common.vo.PageVO;
import cn.lili.modules.distribution.entity.dos.Distribution;
import cn.lili.modules.distribution.entity.dto.DistributionApplyDTO;
import cn.lili.modules.distribution.entity.dto.DistributionSearchParams;
import cn.lili.modules.distribution.entity.enums.DistributionStatusEnum;
import cn.lili.modules.distribution.mapper.DistributionMapper;
import cn.lili.modules.distribution.service.DistributionService;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.service.MemberService;
import cn.lili.modules.system.entity.dos.Setting;
import cn.lili.modules.system.entity.dto.DistributionSetting;
import cn.lili.modules.system.entity.enums.SettingEnum;
import cn.lili.modules.system.service.SettingService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;
import java.util.concurrent.TimeUnit;

/**
 * 分销员接口实现
 *
 * @author pikachu
 * @since 2020-03-14 23:04:56
 */
@Service
public class DistributionServiceImpl extends ServiceImpl<DistributionMapper, Distribution> implements DistributionService {

    /**
     * 会员
     */
    @Autowired
    private MemberService memberService;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 设置
     */
    @Autowired
    private SettingService settingService;

    @Override
    public IPage<Distribution> distributionPage(DistributionSearchParams distributionSearchParams, PageVO page) {
        return this.page(PageUtil.initPage(page), distributionSearchParams.queryWrapper());
    }

    @Override
    public Distribution getDistribution() {

        return this.getOne(new LambdaQueryWrapper<Distribution>().eq(Distribution::getMemberId, UserContext.getCurrentUser().getId()));

    }

    @Override
    public Distribution applyDistribution(DistributionApplyDTO distributionApplyDTO) {

        //检查分销开关
        checkDistributionSetting();

        //判断用户是否申请过分销
        Distribution distribution = getDistribution();

        //如果分销员非空并未审核则提示用户请等待，如果分销员为拒绝状态则重新提交申请
        if (Optional.ofNullable(distribution).isPresent()) {
            if (distribution.getDistributionStatus().equals(DistributionStatusEnum.APPLY.name())) {
                throw new ServiceException(ResultCode.DISTRIBUTION_IS_APPLY);
            } else if (distribution.getDistributionStatus().equals(DistributionStatusEnum.REFUSE.name())) {
                distribution.setDistributionStatus(DistributionStatusEnum.APPLY.name());
                BeanUtil.copyProperties(distributionApplyDTO, distribution);
                this.updateById(distribution);
                return distribution;
            }
        }
        //如果未申请分销员则新增进行申请
        //获取当前登录用户
        Member member = memberService.getUserInfo();
        //新建分销员
        distribution = new Distribution(member.getId(), member.getNickName(), distributionApplyDTO);
        //添加分销员
        this.save(distribution);

        return distribution;
    }

    @Override
    public boolean audit(String id, String status) {

        //检查分销开关
        checkDistributionSetting();

        //根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            if (status.equals(DistributionStatusEnum.PASS.name())) {
                distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
            } else {
                distribution.setDistributionStatus(DistributionStatusEnum.REFUSE.name());
            }
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public boolean retreat(String id) {

        //检查分销开关
        checkDistributionSetting();

        //根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            distribution.setDistributionStatus(DistributionStatusEnum.RETREAT.name());
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public boolean resume(String id) {

        //检查分销开关
        checkDistributionSetting();

        //根据id获取分销员
        Distribution distribution = this.getById(id);
        if (Optional.ofNullable(distribution).isPresent()) {
            distribution.setDistributionStatus(DistributionStatusEnum.PASS.name());
            return this.updateById(distribution);
        }
        return false;
    }

    @Override
    public void bindingDistribution(String distributionId) {

        //判断用户是否登录，未登录不能进行绑定
        if (UserContext.getCurrentUser() == null) {
            throw new ServiceException(ResultCode.USER_NOT_LOGIN);
        }
        //储存分销关系时间
        Distribution distribution = this.getById(distributionId);
        if (distribution != null) {
            Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTING.name());
            DistributionSetting distributionSetting = JSONUtil.toBean(setting.getSettingValue(), DistributionSetting.class);
            cache.put(CachePrefix.DISTRIBUTION.getPrefix() + "_" + UserContext.getCurrentUser().getId(), distribution.getId(), distributionSetting.getDistributionDay().longValue(), TimeUnit.DAYS);
        }

    }

    /**
     * 检查分销设置开关
     */
    @Override
    public void checkDistributionSetting() {
        //获取分销是否开启
        Setting setting = settingService.get(SettingEnum.DISTRIBUTION_SETTING.name());
        DistributionSetting distributionSetting = JSONUtil.toBean(setting.getSettingValue(), DistributionSetting.class);
        if (!distributionSetting.getIsOpen()) {
            throw new ServiceException(ResultCode.DISTRIBUTION_CLOSE);
        }
    }

    @Override
    public void subCanRebate(Double canRebate, String distributionId) {
        this.baseMapper.subCanRebate(canRebate, distributionId);
    }

    @Override
    public void addRebate(Double rebate, String distributionId) {
        this.baseMapper.addCanRebate(rebate, distributionId);
    }

}