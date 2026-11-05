package cn.lili.modules.member.service;

import cn.lili.modules.member.entity.dos.MemberBenefit;
import java.util.List;

/**
 * 客户权益公共查询接口，供普通等级及付费会员复用，不依赖会员资格或权益发放服务。
 *
 * @author Bulbasaur
 * @since 2026-11-05
 */
public interface MemberBenefitQueryService {

    /**
     * 按主键集合查询权益，排除已移除的会员折扣类型，按权益排序值而非入参顺序返回。
     * @param ids 权益主键列表，空值或空列表返回空列表
     * @param onlyOpen true仅返回OPEN状态，其余值不按启停状态过滤
     * @return 匹配的权益列表，不包含已逻辑删除的记录
     */
    List<MemberBenefit> listByIdList(List<String> ids, Boolean onlyOpen);
}
