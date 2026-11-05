package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.MemberBenefitDelivery;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.*;

/**
 * 客户权益发放账本数据层，负责发放任务占位、并发锁定及失败结果记录。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public interface MemberBenefitDeliveryMapper extends BaseMapper<MemberBenefitDelivery> {
    /** 按业务唯一键创建待发放记录；重复记录忽略插入，返回 0，不覆盖历史快照和结果。 */
    @Insert("INSERT IGNORE INTO li_member_benefit_delivery "
        + "(id,member_id,source_type,source_ref,benefit_id,benefit_name,benefit_type,benefit_config,cycle_key,status,attempts,create_time,update_time,delete_flag) "
        + "VALUES (#{id},#{memberId},#{sourceType},#{sourceRef},#{benefitId},#{benefitName},#{benefitType},#{benefitConfig},#{cycleKey},'PENDING',0,NOW(),NOW(),0)")
    int reserve(MemberBenefitDelivery record);

    /** 在当前事务中锁定一条发放记录，串行处理同一权益任务，避免并发重复发放。 */
    @Select("SELECT * FROM li_member_benefit_delivery WHERE id=#{id} FOR UPDATE")
    MemberBenefitDelivery lock(@Param("id") String id);

    /** 记录失败原因并累加尝试次数；不允许将已成功的记录改回失败。 */
    @Update("UPDATE li_member_benefit_delivery SET status='FAILED', attempts=attempts+1,error_message=#{error},update_time=NOW() WHERE id=#{id} AND status<>'SUCCESS'")
    void failed(@Param("id") String id, @Param("error") String error);
}
