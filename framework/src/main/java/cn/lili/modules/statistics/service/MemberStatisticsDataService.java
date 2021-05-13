package cn.lili.modules.statistics.service;

import cn.lili.modules.statistics.model.dos.MemberStatisticsData;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.List;

/**
 * 会员统计业务层
 *
 * @author Bulbasaur
 * @date 2020/12/9 11:06
 */
public interface MemberStatisticsDataService extends IService<MemberStatisticsData> {

    /**
     * 获取会员数量
     *
     * @return 会员统计
     */
    Integer getMemberCount();

    /**
     * 获取今日新增会员数量
     *
     * @return 今日新增会员数量
     */
    Integer todayMemberNum();

    /**
     * 获取指定结束时间前的会员数量
     *
     * @param endTime
     * @return
     */
    Integer memberCount(Date endTime);

    /**
     * 当天活跃会员数量
     *
     * @param startTime
     * @return
     */
    Integer activeQuantity(Date startTime);

    /**
     * 时间段内新增会员数量
     *
     * @param endTime
     * @param startTime
     * @return
     */
    Integer newlyAdded(Date endTime, Date startTime);

    /**
     * 根据参数，查询这段时间的会员统计
     *
     * @param statisticsQueryParam
     * @return
     */
    List<MemberStatisticsData> statistics(StatisticsQueryParam statisticsQueryParam);
}