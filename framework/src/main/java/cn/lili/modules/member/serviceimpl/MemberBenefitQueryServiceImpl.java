package cn.lili.modules.member.serviceimpl;

import cn.hutool.core.collection.CollUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.modules.member.entity.dos.MemberBenefit;
import cn.lili.modules.member.mapper.MemberBenefitMapper;
import cn.lili.modules.member.service.MemberBenefitQueryService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

/**
 * 客户权益公共查询服务，供普通等级及付费会员复用，不依赖会员资格或权益发放服务。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
@Service("memberBenefitQueryService")
public class MemberBenefitQueryServiceImpl implements MemberBenefitQueryService {
    /**
     * 权益配置数据访问接口。
     */
    private final MemberBenefitMapper mapper;

    /**
     * 注入权益存储，保持查询层依赖单向流向数据层。
     *
     * @param mapper 权益配置Mapper
     */
    public MemberBenefitQueryServiceImpl(MemberBenefitMapper mapper) {
        this.mapper = mapper;
    }

    /**
     * 按主键集合查询权益，排除已移除的会员折扣类型，按权益排序值而非入参顺序返回。
     *
     * @param ids      权益主键列表，空值或空列表返回空列表
     * @param onlyOpen true仅返回OPEN状态，其余值不按启停状态过滤
     * @return 匹配的权益列表，不包含已逻辑删除的记录
     */
    @Override
    public List<MemberBenefit> listByIdList(List<String> ids, Boolean onlyOpen) {
        if (CollUtil.isEmpty(ids)) return Collections.emptyList();
        return mapper.selectList(new LambdaQueryWrapper<MemberBenefit>()
                .ne(MemberBenefit::getBenefitType, "MEMBER_DISCOUNT")
                .in(MemberBenefit::getId, ids)
                .eq(Boolean.TRUE.equals(onlyOpen), MemberBenefit::getBenefitState, SwitchEnum.OPEN.name())
                .orderByAsc(MemberBenefit::getBenefitSort));
    }
}
