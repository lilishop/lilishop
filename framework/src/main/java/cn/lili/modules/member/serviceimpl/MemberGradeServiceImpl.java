package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.member.entity.dos.Member;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.entity.dos.MemberGrade;
import cn.lili.modules.member.entity.vo.MemberGradeDetailVO;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.mapper.MemberGradeMapper;
import cn.lili.modules.member.mapper.MemberMapper;
import cn.lili.modules.member.service.MemberGradeService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.update.LambdaUpdateWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.net.URI;
import java.util.*;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * 客户等级业务层实现
 *
 * @author Bulbasaur
 * @since 2021/5/14 5:58 下午
 */
@Service
public class MemberGradeServiceImpl extends ServiceImpl<MemberGradeMapper, MemberGrade> implements MemberGradeService {

    private static final Pattern HEX_COLOR = Pattern.compile("^#([A-Fa-f0-9]{6})$");

    @Autowired
    private MemberMapper memberMapper;
    @Autowired
    private MemberBenefitMapper memberBenefitMapper;

    @Override
    public void saveGrade(MemberGrade memberGrade) {
        if (memberGrade.getIsDefault() == null) {
            memberGrade.setIsDefault(false);
        }
        this.validateGrade(memberGrade, null);
        if (this.count() <= 0) {
            memberGrade.setIsDefault(true);
        }
        if (!this.save(memberGrade)) {
            throw new ServiceException(ResultCode.ERROR, "客户等级保存失败");
        }
        if (Boolean.TRUE.equals(memberGrade.getIsDefault())) {
            this.clearOtherDefault(memberGrade.getId());
        }
    }

    @Override
    public void updateGrade(String id, MemberGrade memberGrade) {
        MemberGrade original = this.getById(id);
        if (original == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST, "客户等级不存在");
        }
        memberGrade.setId(id);
        if (memberGrade.getIsDefault() == null) {
            memberGrade.setIsDefault(Boolean.TRUE.equals(original.getIsDefault()));
        }
        this.validateGrade(memberGrade, id);
        if (Boolean.FALSE.equals(memberGrade.getIsDefault()) && Boolean.TRUE.equals(original.getIsDefault())) {
            if (!this.hasOtherDefault(id)) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "至少保留一个默认等级");
            }
        }
        if (!this.updateById(memberGrade)) {
            throw new ServiceException(ResultCode.ERROR, "客户等级更新失败");
        }
        if (Boolean.TRUE.equals(memberGrade.getIsDefault())) {
            this.clearOtherDefault(id);
        }
    }

    @Override
    public void updateGradeState(String id, String state) {
        if (!SwitchEnum.OPEN.name().equals(state) && !SwitchEnum.CLOSE.name().equals(state)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级开关状态非法");
        }
        MemberGrade memberGrade = this.getById(id);
        if (memberGrade == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST, "客户等级不存在");
        }
        if (Boolean.TRUE.equals(memberGrade.getIsDefault()) && SwitchEnum.CLOSE.name().equals(state)) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "默认等级必须为开启状态");
        }
        memberGrade.setGradeState(state);
        this.updateById(memberGrade);
    }

    @Override
    public MemberGrade getCurrentGradeByExperience(Long experience) {
        long currentExperience = experience == null ? 0L : Math.max(experience, 0L);
        LambdaQueryWrapper<MemberGrade> queryWrapper = new LambdaQueryWrapper<MemberGrade>()
                .eq(MemberGrade::getGradeState, SwitchEnum.OPEN.name())
                .le(MemberGrade::getRequiredExperience, currentExperience)
                .orderByDesc(MemberGrade::getRequiredExperience)
                .last("limit 1");
        MemberGrade grade = this.getOne(queryWrapper, false);
        if (grade != null) {
            return grade;
        }
        return this.getDefaultGrade();
    }

    @Override
    public List<MemberGrade> listForUpgrade() {
        LambdaQueryWrapper<MemberGrade> queryWrapper = new LambdaQueryWrapper<MemberGrade>()
                .orderByAsc(MemberGrade::getGradeSort);
        return this.list(queryWrapper);
    }

    @Override
    public List<MemberGradeDetailVO> listForUpgradeWithBenefits() {
        List<MemberGrade> grades = this.listForUpgrade().stream().filter(g -> SwitchEnum.OPEN.name().equals(g.getGradeState())).collect(Collectors.toList());
        if (CollUtil.isEmpty(grades)) {
            return Collections.emptyList();
        }
        List<MemberGradeDetailVO> result = new ArrayList<>(grades.size());
        for (MemberGrade g : grades) {
            result.add(this.buildGradeDetailVo(g, true));
        }
        return result;
    }

    @Override
    public MemberGradeDetailVO getManagerGradeDetail(String id) {
        MemberGrade grade = this.getById(id);
        if (grade == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST, "客户等级不存在");
        }
        return this.buildGradeDetailVo(grade, false);
    }

    /**
     * 组装等级及关联权益，按等级配置的权益ID顺序排列，过滤不存在或不支持的权益类型。
     *
     * @param grade            待展示等级
     * @param onlyOpenBenefits true仅保留OPEN权益，用于买家端；false用于管理端回显
     * @return 等级及关联权益详情
     */
    private MemberGradeDetailVO buildGradeDetailVo(MemberGrade grade, boolean onlyOpenBenefits) {
        MemberGradeDetailVO vo = new MemberGradeDetailVO();
        vo.setGrade(grade);
        List<String> ids = parseBenefitIdList(grade.getBenefitIds());
        if (CollUtil.isEmpty(ids)) {
            vo.setBenefits(Collections.emptyList());
            return vo;
        }
        List<MemberBenefit> found = this.memberBenefitMapper.selectList(new LambdaQueryWrapper<MemberBenefit>()
                .in(MemberBenefit::getId, ids));
        Map<String, MemberBenefit> byId = found.stream()
                .collect(Collectors.toMap(MemberBenefit::getId, Function.identity(), (a, b) -> a));
        List<MemberBenefit> ordered = new ArrayList<>();
        for (String bid : ids) {
            MemberBenefit b = byId.get(bid);
            if (b == null || !cn.lili.modules.member.entity.enums.MemberBenefitTypeEnum.isValid(b.getBenefitType())) {
                continue;
            }
            if (onlyOpenBenefits && !SwitchEnum.OPEN.name().equals(b.getBenefitState())) {
                continue;
            }
            if (onlyOpenBenefits)
                b.setBenefitDesc(cn.lili.modules.member.util.MemberBenefitRules.description(b.getBenefitDesc()));
            ordered.add(b);
        }
        vo.setBenefits(ordered);
        return vo;
    }

    @Override
    public void deleteGrade(String id) {
        MemberGrade memberGrade = this.getById(id);
        if (memberGrade == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST, "客户等级不存在");
        }
        if (Boolean.TRUE.equals(memberGrade.getIsDefault())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "默认等级不支持删除");
        }
        LambdaQueryWrapper<Member> memberWrapper = new LambdaQueryWrapper<Member>()
                .eq(Member::getGradeId, id);
        long count = memberMapper.selectCount(memberWrapper);
        if (count > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "当前等级下存在会员，无法删除");
        }
        if (!this.removeById(id)) {
            throw new ServiceException(ResultCode.ERROR, "客户等级删除失败");
        }
    }

    @Override
    public void updateGradeBenefits(String gradeId, List<String> benefitIds) {
        MemberGrade memberGrade = this.getById(gradeId);
        if (memberGrade == null) {
            throw new ServiceException(ResultCode.USER_NOT_EXIST, "客户等级不存在");
        }
        List<String> normalized = this.normalizeBenefitIds(benefitIds);
        if (CollUtil.isNotEmpty(normalized)) {
            Long count = memberBenefitMapper.selectCount(new LambdaQueryWrapper<MemberBenefit>().ne(MemberBenefit::getBenefitType, "MEMBER_DISCOUNT")
                    .in(MemberBenefit::getId, normalized));
            long actual = count == null ? 0L : count;
            if (actual != normalized.size()) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "存在无效的权益ID");
            }
            memberGrade.setBenefitIds(String.join(",", normalized));
        } else {
            memberGrade.setBenefitIds(null);
        }
        if (!this.updateById(memberGrade)) {
            throw new ServiceException(ResultCode.ERROR, "客户等级权益更新失败");
        }
    }

    @Override
    public List<String> getGradeBenefitIdList(String gradeId) {
        MemberGrade memberGrade = this.getById(gradeId);
        if (memberGrade == null) {
            return new ArrayList<>();
        }
        /**
         * 拆分逗号分隔的权益主键，去掉空白项并保留配置顺序，此处不去重。
         *
         * @param benefitIds 等级配置的权益主键字符串
         * @return 权益主键列表，空白配置返回空列表
         */
        return parseBenefitIdList(memberGrade.getBenefitIds());
    }

    /**
     * 校验等级名称、经验门槛、排序、启停、图标、背景、字体色及关联权益。
     * 默认等级必须启用；名称和经验门槛不可与其他等级重复，权益ID归一化后原地写回。
     *
     * @param memberGrade 待保存等级
     * @param ignoreId    更新时排除自身的等级主键，新增时可为空
     * @throws ServiceException 必填信息、状态、格式、关联权益或唯一性不合法
     */
    private void validateGrade(MemberGrade memberGrade, String ignoreId) {
        if (memberGrade == null) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "客户等级参数不能为空");
        }
        if (CharSequenceUtil.isBlank(memberGrade.getGradeName())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级名称不能为空");
        }
        if (memberGrade.getRequiredExperience() == null || memberGrade.getRequiredExperience() < 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "所需经验值必须为正整数");
        }
        if (memberGrade.getGradeSort() == null || memberGrade.getGradeSort() < 1) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级排序必须为正整数");
        }
        if (!SwitchEnum.OPEN.name().equals(memberGrade.getGradeState()) && !SwitchEnum.CLOSE.name().equals(memberGrade.getGradeState())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级开关状态非法");
        }
        if (Boolean.TRUE.equals(memberGrade.getIsDefault()) && SwitchEnum.CLOSE.name().equals(memberGrade.getGradeState())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "默认等级必须为开启状态");
        }
        if (CharSequenceUtil.isBlank(memberGrade.getGradeImage())) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级图标不能为空");
        }
        this.validateUrl(memberGrade.getGradeImage(), "等级图标URL格式错误");
        this.validateOptionalUrl(memberGrade.getGradeBackground(), "等级背景图URL格式错误");
        this.validateOptionalColor(memberGrade.getGradeFontColor(), "等级字体颜色格式错误，仅支持#RRGGBB");

        List<String> benefitIdList = parseBenefitIdList(memberGrade.getBenefitIds());
        List<String> normalizedBenefitIdList = normalizeBenefitIds(benefitIdList);
        if (CollUtil.isNotEmpty(normalizedBenefitIdList)) {
            Long count = memberBenefitMapper.selectCount(new LambdaQueryWrapper<MemberBenefit>().ne(MemberBenefit::getBenefitType, "MEMBER_DISCOUNT")
                    .in(MemberBenefit::getId, normalizedBenefitIdList));
            long actual = count == null ? 0L : count;
            if (actual != normalizedBenefitIdList.size()) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, "存在无效的权益ID");
            }
            memberGrade.setBenefitIds(String.join(",", normalizedBenefitIdList));
        } else {
            memberGrade.setBenefitIds(null);
        }

        LambdaQueryWrapper<MemberGrade> nameWrapper = new LambdaQueryWrapper<MemberGrade>()
                .eq(MemberGrade::getGradeName, memberGrade.getGradeName());
        if (CharSequenceUtil.isNotBlank(ignoreId)) {
            nameWrapper.ne(MemberGrade::getId, ignoreId);
        }
        if (this.count(nameWrapper) > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "等级名称不能重复");
        }

        LambdaQueryWrapper<MemberGrade> expWrapper = new LambdaQueryWrapper<MemberGrade>()
                .eq(MemberGrade::getRequiredExperience, memberGrade.getRequiredExperience());
        if (CharSequenceUtil.isNotBlank(ignoreId)) {
            expWrapper.ne(MemberGrade::getId, ignoreId);
        }
        if (this.count(expWrapper) > 0) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, "所需经验值不能重复");
        }
    }

    private static List<String> parseBenefitIdList(String benefitIds) {
        if (CharSequenceUtil.isBlank(benefitIds)) {
            return new ArrayList<>();
        }
        List<String> parts = CharSequenceUtil.splitTrim(benefitIds, ',');
        List<String> result = new ArrayList<>();
        if (CollUtil.isEmpty(parts)) {
            return result;
        }
        for (String part : parts) {
            if (CharSequenceUtil.isNotBlank(part)) {
                result.add(part);
            }
        }
        return result;
    }

    /**
     * 清理权益主键首尾空白并去重，保留首次出现的顺序。
     *
     * @param benefitIds 待归一化的权益主键列表
     * @return 去重后的主键列表，空入参返回空列表
     */
    private List<String> normalizeBenefitIds(List<String> benefitIds) {
        if (CollUtil.isEmpty(benefitIds)) {
            return new ArrayList<>();
        }
        Set<String> set = new LinkedHashSet<>();
        for (String id : benefitIds) {
            if (CharSequenceUtil.isNotBlank(id)) {
                set.add(id.trim());
            }
        }
        return new ArrayList<>(set);
    }

    /**
     * 将指定等级以外的现有默认等级取消默认标记，供设置新默认等级时调用。
     *
     * @param id 需要保留默认标记的等级主键
     */
    private void clearOtherDefault(String id) {
        LambdaUpdateWrapper<MemberGrade> updateWrapper = new LambdaUpdateWrapper<MemberGrade>()
                .set(MemberGrade::getIsDefault, false)
                .ne(MemberGrade::getId, id)
                .eq(MemberGrade::getIsDefault, true);
        this.update(updateWrapper);
    }

    /**
     * 查询除当前等级外是否还存在默认等级，不额外过滤启停状态。
     *
     * @param ignoreId 需要排除的当前等级主键，可为null
     * @return 是否存在其他默认等级
     */
    private boolean hasOtherDefault(String ignoreId) {
        LambdaQueryWrapper<MemberGrade> wrapper = new LambdaQueryWrapper<MemberGrade>()
                .eq(MemberGrade::getIsDefault, true)
                .ne(Objects.nonNull(ignoreId), MemberGrade::getId, ignoreId);
        return this.count(wrapper) > 0;
    }

    /**
     * 查询处于OPEN状态的默认等级。
     *
     * @return 启用的默认等级，不存在时为null
     */
    private MemberGrade getDefaultGrade() {
        LambdaQueryWrapper<MemberGrade> wrapper = new LambdaQueryWrapper<MemberGrade>()
                .eq(MemberGrade::getGradeState, SwitchEnum.OPEN.name())
                .eq(MemberGrade::getIsDefault, true)
                .last("limit 1");
        return this.getOne(wrapper, false);
    }

    /**
     * 校验可选图片地址，空白值不校验，非空时执行统一地址校验。
     *
     * @param url     图片地址
     * @param message 校验失败提示
     * @throws ServiceException 地址不符合格式
     */
    private void validateOptionalUrl(String url, String message) {
        if (CharSequenceUtil.isBlank(url)) {
            return;
        }
        /**
         * 校验图片地址，兼容约定的本地等级/权益PNG路径，其他地址须同时包含协议和主机。
         * 本方法只检查格式，不访问远程资源。
         *
         * @param url 待校验图片地址
         * @param message 校验失败提示
         * @throws ServiceException 地址解析失败或缺少协议、主机
         */
        validateUrl(url, message);
    }

    private void validateUrl(String url, String message) {
        if (url != null && url.matches("^/static/member-benefits/[a-z0-9-]+\\.png$")) return;
        try {
            URI uri = URI.create(url);
            if (CharSequenceUtil.isBlank(uri.getScheme()) || CharSequenceUtil.isBlank(uri.getHost())) {
                throw new ServiceException(ResultCode.PARAMS_ERROR, message);
            }
        } catch (Exception e) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, message);
        }
    }

    /**
     * 校验可选字体颜色，非空时仅允许#RRGGBB六位十六进制格式。
     *
     * @param color   待校验色值，空白不校验
     * @param message 校验失败提示
     * @throws ServiceException 颜色格式不合法
     */
    private void validateOptionalColor(String color, String message) {
        if (CharSequenceUtil.isBlank(color)) {
            return;
        }
        if (!HEX_COLOR.matcher(color).matches()) {
            throw new ServiceException(ResultCode.PARAMS_ERROR, message);
        }
    }
}
