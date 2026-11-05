package cn.lili.modules.member.mapper;

import cn.lili.modules.member.entity.dos.MemberPointsHistory;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.Select;
import org.apache.ibatis.annotations.Param;

/**
 * 客户积分历史数据处理层
 *
 * @author Bulbasaur
 * @since 2020-02-25 14:10:16
 */
public interface MemberPointsHistoryMapper extends BaseMapper<MemberPointsHistory> {

    /**
     * 读取本订单已存在的购物赠送积分流水，兼容此前完成时发放的说明格式。
     * 供单任务发放流程判断是否已赠送，不额外加锁；流水说明格式为防重依据，不可随意改写。
     * @param memberId 客户主键
     * @param orderSn 订单业务单号
     * @return 已发流水主键，不存在时为null
     */
    @Select("SELECT id FROM li_member_points_history WHERE member_id=#{memberId} AND point_type='INCREASE' AND point_source='ORDER' "
            + "AND content IN (CONCAT('订单[',#{orderSn},']售后期结束，购物赠送积分'),CONCAT('订单[',#{orderSn},']完成，会员购物赠送积分')) LIMIT 1")
    String findOrderReward(@Param("memberId") String memberId, @Param("orderSn") String orderSn);

}