package cn.lili.modules.promotion.mapper;
import cn.lili.modules.promotion.entity.dos.LotteryActivity;
import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import org.apache.ibatis.annotations.*;

/**
 * 抽奖活动存储；行锁串行化奖池配置、库存发放及上下架。
 *
 * @author Bulbasaur
 * @since 2026-11-14
 */
public interface LotteryActivityMapper extends BaseMapper<LotteryActivity> {
    /**
     * 锁定活动至事务结束。
     *
     * @param id 活动ID
     * @return 未删除活动
     */
    @Select("SELECT * FROM li_lottery_activity WHERE id=#{id} AND delete_flag=0 FOR UPDATE")
    LotteryActivity lock(@Param("id") String id);
}
