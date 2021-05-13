package cn.lili.modules.statistics.serviceimpl;

import cn.hutool.core.date.DateUtil;
import cn.lili.common.enums.SwitchEnum;
import cn.lili.modules.statistics.mapper.MemberStatisticsDataMapper;
import cn.lili.modules.statistics.model.dos.MemberStatisticsData;
import cn.lili.modules.statistics.model.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.service.MemberStatisticsDataService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Date;
import java.util.List;

/**
 * 会员统计业务层实现
 *
 * @author Bulbasaur
 * @date 2020/12/9 18:33
 */
@Service
public class MemberStatisticsDataServiceImpl extends ServiceImpl<MemberStatisticsDataMapper, MemberStatisticsData> implements MemberStatisticsDataService {

    /**
     * 会员统计
     */
    @Autowired
    private MemberStatisticsDataMapper memberStatisticsDataMapper;

    @Override
    public Integer getMemberCount() {
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("disabled", SwitchEnum.OPEN.name());
        return memberStatisticsDataMapper.customSqlQuery(queryWrapper);
    }

    @Override
    public Integer todayMemberNum() {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.gt("create_time", DateUtil.beginOfDay(new Date()));
        return memberStatisticsDataMapper.customSqlQuery(queryWrapper);
    }

    @Override
    public Integer memberCount(Date endTime) {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.lt("create_time", endTime);
        return memberStatisticsDataMapper.customSqlQuery(queryWrapper);
    }

    @Override
    public Integer activeQuantity(Date startTime) {

        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.ge("last_login_date", startTime);
        return memberStatisticsDataMapper.customSqlQuery(queryWrapper);
    }

    @Override
    public Integer newlyAdded(Date startTime, Date endTime) {
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.between("create_time", startTime, endTime);
        return memberStatisticsDataMapper.customSqlQuery(queryWrapper);
    }

    @Override
    public List<MemberStatisticsData> statistics(StatisticsQueryParam statisticsQueryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(statisticsQueryParam);
        Date startTime = dates[0], endTime = dates[1];
        QueryWrapper queryWrapper = Wrappers.query();
        queryWrapper.between("create_date", startTime, endTime);

        return list(queryWrapper);
    }
}
