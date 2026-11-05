package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.MemberGradeBenefitGrant;
import cn.lili.modules.member.entity.vo.MemberBenefitGrantRecordVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.core.toolkit.Constants;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

/**
 * 客户等级权益初始化记录查询；初始化记录不代表所有权益均已发放成功。
 *
 * @author Bulbasaur
 * @since 2026-11-04
 */
public interface MemberGradeBenefitGrantMapper extends BaseMapper<MemberGradeBenefitGrant> {

    /**
     * 分页查询等级权益初始化记录，关联客户账号、手机号及等级名称。
     * 具体权益发放结果应查询权益发放明细。
     *
     * @param page 分页参数
     * @param wrapper 服务端构建的筛选及排序条件，禁止直接接收用户 SQL
     * @return 初始化记录分页
     */
    @Select("SELECT g.id, g.member_id AS memberId, g.grade_id AS gradeId, g.create_time AS createTime, "
            + "m.mobile AS memberMobile, m.username AS memberUsername, mgr.grade_name AS gradeName "
            + "FROM li_member_grade_benefit_grant g "
            + "LEFT JOIN li_member m ON g.member_id = m.id "
            + "LEFT JOIN li_member_grade mgr ON g.grade_id = mgr.id "
            + "${ew.customSqlSegment}")
    IPage<MemberBenefitGrantRecordVO> pageGrantRecords(IPage<MemberBenefitGrantRecordVO> page,
                                                      @Param(Constants.WRAPPER) Wrapper<?> wrapper);
}
