package cn.lili.modules.promotion.mapper;
import cn.lili.modules.promotion.entity.dos.LotteryPrize;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.*;
import java.util.List;

/**
 * 奖项存储，抽奖在活动锁内进行当前读，避免旧快照导致超发。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
public interface LotteryPrizeMapper extends BaseMapper<LotteryPrize> {
    /**
     * 读取并锁定有序奖池。
     *
     * @param id 活动ID
     * @return 奖项列表
     */
    @Select("SELECT * FROM li_lottery_prize WHERE activity_id=#{id} AND delete_flag=0 ORDER BY sort_order FOR UPDATE")
    List<LotteryPrize> lockPool(@Param("id") String id);
}
