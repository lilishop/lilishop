package cn.lili.modules.promotion.mapper;
import cn.lili.modules.promotion.entity.dos.LotteryRecord;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.*;

/**
 * 抽奖流水及会员级请求去重存储。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
public interface LotteryRecordMapper extends BaseMapper<LotteryRecord> {
    /**
     * 当前读原结果，重试跨天也不重新发奖。
     *
     * @param member 会员ID
     * @param request 请求号
     * @return 原结果
     */
    @Select("SELECT * FROM li_lottery_record WHERE member_id=#{member} AND request_id=#{request} FOR UPDATE")
    LotteryRecord findRequest(@Param("member") String member, @Param("request") String request);
}
