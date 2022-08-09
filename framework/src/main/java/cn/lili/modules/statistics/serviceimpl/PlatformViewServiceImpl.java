package cn.lili.modules.statistics.serviceimpl;

import cn.lili.cache.Cache;
import cn.lili.cache.CachePrefix;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.properties.StatisticsProperties;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.utils.CurrencyUtil;
import cn.lili.modules.member.entity.vo.MemberDistributionVO;
import cn.lili.modules.statistics.entity.dos.PlatformViewData;
import cn.lili.modules.statistics.entity.dto.StatisticsQueryParam;
import cn.lili.modules.statistics.entity.enums.SearchTypeEnum;
import cn.lili.modules.statistics.entity.vo.OnlineMemberVO;
import cn.lili.modules.statistics.entity.vo.PlatformViewVO;
import cn.lili.modules.statistics.mapper.PlatformViewMapper;
import cn.lili.modules.statistics.service.MemberStatisticsService;
import cn.lili.modules.statistics.service.PlatformViewService;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import cn.lili.modules.statistics.util.StatisticsSuffix;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

/**
 * 流量统计
 *
 * @author Chopper
 * @version v1.0
 * @since v7.0
 * 2021/1/18 12:07
 */
@Service
public class PlatformViewServiceImpl extends ServiceImpl<PlatformViewMapper, PlatformViewData> implements PlatformViewService {

    /**
     * 在线人数统计
     */
    @Autowired
    private StatisticsProperties statisticsProperties;
    /**
     * 会员
     */
    @Autowired
    private MemberStatisticsService memberStatisticsService;
    /**
     * 缓存
     */
    @Autowired
    private Cache cache;
    /**
     * 平台流量统计
     */
    @Resource
    private PlatformViewMapper platformViewMapper;

    @Override
    public Long online() {
        Object object = cache.get(CachePrefix.ONLINE_NUM.getPrefix());

        if (null != object) {
            return (Long) object;
        }
        //这里统计的是有效的accessToken ，如果需要数据精确，需要调整accessToken的有效时间，开发人员建议2小时误差较为合适
        Long num = Long.valueOf(cache.keys(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MEMBER) + "*").size());
        cache.put(CachePrefix.ONLINE_NUM.getPrefix(), num, statisticsProperties.getCurrentOnlineUpdate().longValue());
        return num;
    }

    @Override
    public List<MemberDistributionVO> memberDistribution() {
        Object object = cache.get(CachePrefix.MEMBER_DISTRIBUTION.getPrefix());

        if (null != object) {
            return (List<MemberDistributionVO>) object;
        }
        List<MemberDistributionVO> memberDistributionVOS = memberStatisticsService.distribution();

        //统计总数
        int count = 0;
        for (MemberDistributionVO vo : memberDistributionVOS) {
            count += vo.getNum();
        }
        //初始化数据，填充枚举和比例
        for (MemberDistributionVO vo : memberDistributionVOS) {
            vo.setProportion(CurrencyUtil.div(vo.getNum(), count, 4));
            //客户端填充
            if (StringUtils.isNotEmpty(vo.getClientEnum())) {
                vo.setClientEnum(ClientTypeEnum.valueOf(vo.getClientEnum()).clientName());
            } else {
                vo.setClientEnum(ClientTypeEnum.UNKNOWN.clientName());
            }
        }

        cache.put(CachePrefix.MEMBER_DISTRIBUTION.getPrefix(), memberDistributionVOS);
        return memberDistributionVOS;
    }

    @Override
    public List<OnlineMemberVO> statisticsOnline() {
        Object object = cache.get(CachePrefix.ONLINE_MEMBER.getPrefix());
        List<OnlineMemberVO> result = new ArrayList<>();
        if (object != null) {
            result = (List<OnlineMemberVO>) object;
        }
        return this.initData(result);
    }

    /**
     * 在线会员数据初始化
     *
     * @param source
     * @return
     */
    private List<OnlineMemberVO> initData(List<OnlineMemberVO> source) {
        List<OnlineMemberVO> onlineMemberVOS = new ArrayList<>();

        Calendar calendar = Calendar.getInstance();
        calendar.set(Calendar.MINUTE, 0);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);

        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - statisticsProperties.getOnlineMember());
        //循环填充数据
        for (int i = 0; i < statisticsProperties.getOnlineMember(); i++) {
            calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) + 1);
            OnlineMemberVO exitOnlineNum = null;
            for (int j = 0; j < source.size(); j++) {
                if (source.get(j).getDate().equals(calendar.getTime())) {
                    exitOnlineNum = source.get(j);
                }
            }
            if (exitOnlineNum == null) {
                onlineMemberVOS.add(new OnlineMemberVO(calendar.getTime(), 0, 0));
            } else {
                onlineMemberVOS.add(exitOnlineNum);
            }
        }
        return onlineMemberVOS;
    }

    @Override
    public List<PlatformViewVO> list(StatisticsQueryParam queryParam) {
        List<PlatformViewVO> result = new ArrayList<>();

        //查询开始时间和结束时间，用于数据库查询
        Date endTime = null, startTime = null;

        //搜索类型判定，如果不为空，则按照搜索类型进行查询
        if (StringUtils.isNotEmpty(queryParam.getSearchType())) {
            SearchTypeEnum searchTypeEnum = SearchTypeEnum.valueOf(queryParam.getSearchType());
            switch (searchTypeEnum) {
                case TODAY:
                    PlatformViewVO today = new PlatformViewVO();
                    //查询 平台流量
                    if (StringUtils.isEmpty(queryParam.getStoreId())) {
                        //设置PV UV属性
                        String pv = cache.getString(CachePrefix.PV.getPrefix() + StatisticsSuffix.suffix());
                        if (pv == null) {
                            pv = "0";
                        }
                        today.setPvNum(Long.valueOf(pv));
                        today.setUvNum(cache.counter(CachePrefix.UV.getPrefix() + StatisticsSuffix.suffix()).longValue());
                    }
                    //店铺流量
                    else {
                        //设置PV UV属性

                        String pv = cache.getString(CachePrefix.STORE_PV.getPrefix() + StatisticsSuffix.suffix(queryParam.getStoreId()));
                        if (pv == null) {
                            pv = "0";
                        }
                        today.setPvNum(Long.valueOf(pv));
                        today.setUvNum(cache.counter(CachePrefix.STORE_UV.getPrefix() + StatisticsSuffix.suffix(queryParam.getStoreId())).longValue());
                    }
                    today.setDate(new Date());
                    result.add(today);
                    break;
                case YESTERDAY:
                case LAST_SEVEN:
                case LAST_THIRTY: {
                    Date[] dates = StatisticsDateUtil.getDateArray(searchTypeEnum);
                    endTime = dates[1];
                    startTime = dates[0];
                    break;
                }
                default:
                    throw new ServiceException(ResultCode.ERROR);
            }
        } else {
            //根据查询时间来确定查询参数
            Integer year = queryParam.getYear();
            Integer month = queryParam.getMonth();

            Calendar calendar = Calendar.getInstance();
            calendar.set(Calendar.HOUR_OF_DAY, 0);
            calendar.set(Calendar.MINUTE, 0);
            calendar.set(Calendar.SECOND, 0);
            calendar.set(Calendar.MILLISECOND, 0);

            calendar.set(year, month - 1, 1);
            startTime = calendar.getTime();
            calendar.set(year, month, -1);
            endTime = calendar.getTime();
        }
        //时间不为空则按照时间开始数据查询
        if (startTime != null) {
            LambdaQueryWrapper<PlatformViewData> lambdaQueryWrapper = new LambdaQueryWrapper<>();
            lambdaQueryWrapper.between(PlatformViewData::getDate, startTime, endTime);
            lambdaQueryWrapper.eq(PlatformViewData::getStoreId, StringUtils.isEmpty(queryParam.getStoreId()) ?
                    "-1" : queryParam.getStoreId());
            List<PlatformViewData> dataList = this.list(lambdaQueryWrapper);
            result = builderVOS(startTime, endTime, dataList);
        }
        return result;
    }

    @Override
    public Integer countUv(StatisticsQueryParam queryParam) {
        Date[] dates = StatisticsDateUtil.getDateArray(queryParam);
        //获取当前时间
        Calendar calendar = Calendar.getInstance();


        calendar.set(calendar.get(Calendar.YEAR), calendar.get(Calendar.MONTH), calendar.get(Calendar.DAY_OF_MONTH), 0, 0, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        //如果是今天的统计，则从redis 中拿，否则从数据库中拿
        if (dates[0].equals(calendar.getTime())) {
            if (StringUtils.isNotEmpty(queryParam.getStoreId())) {
                return cache.counter(CachePrefix.UV.getPrefix() + StatisticsSuffix.suffix(queryParam.getStoreId())).intValue();
            }
            return cache.counter(CachePrefix.UV.getPrefix() + StatisticsSuffix.suffix()).intValue();
        } else {
            QueryWrapper queryWrapper = new QueryWrapper();
            queryWrapper.between("date", dates[0], dates[1]);
            //根据店铺查询判定，如果有，则店铺查询，如果没有，则根据商家查询
            if (StringUtils.isNotEmpty(queryParam.getStoreId())) {
                queryWrapper.eq("store_id", queryParam.getStoreId());
            } else {
                queryWrapper.eq("store_id", -1);
            }
            return platformViewMapper.count(queryWrapper);
        }
    }

    /**
     * 根据查询条件，创建数据
     *
     * @param startDate
     * @param endDate
     * @param dataList
     * @return
     */
    private List<PlatformViewVO> builderVOS(Date startDate, Date endDate, List<PlatformViewData> dataList) {

        Calendar startTime = Calendar.getInstance();
        startTime.setTime(startDate);

        Calendar endTime = Calendar.getInstance();
        endTime.setTime(endDate);

        List<PlatformViewVO> result = new ArrayList<>();

        //构造所有需要统计展示等流量数据
        List<Date> listDate = new ArrayList<>();
        while (startTime.before(endTime) || startTime.getTime().equals(endTime.getTime())) {
            listDate.add(startTime.getTime());
            startTime.set(Calendar.DAY_OF_MONTH, startTime.get(Calendar.DAY_OF_MONTH) + 1);

        }
        //根据时间集，匹配查询到等数据，构建返回等VO
        listDate.forEach(date -> {
            PlatformViewVO platformViewVO = new PlatformViewVO(date);
            dataList.forEach(platformViewData -> {
                if (platformViewData.getDate().equals(date)) {
                    BeanUtils.copyProperties(platformViewData, platformViewVO);
                }
            });
            //没有匹配到数据库查询的数据，则初始化数据
            result.add(platformViewVO);
        });
        return result;

    }

}
