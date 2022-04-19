package cn.lili.modules.search.serviceimpl;

import cn.lili.modules.search.entity.dos.HotWordsHistory;
import cn.lili.modules.search.entity.dto.HotWordsSearchParams;
import cn.lili.modules.search.mapper.HotWordsHistoryMapper;
import cn.lili.modules.search.service.HotWordsHistoryService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;

import java.util.*;

/**
 * 历史热词
 *
 * @author paulG
 * @since 2020/10/15
 **/
@Service
public class HotWordsHistoryServiceImpl extends ServiceImpl<HotWordsHistoryMapper, HotWordsHistory> implements HotWordsHistoryService {

    @Override
    public List<HotWordsHistory> statistics(HotWordsSearchParams hotWordsSearchParams) {
        QueryWrapper queryWrapper = hotWordsSearchParams.queryWrapper();

        List<HotWordsHistory> list = baseMapper.statistics(queryWrapper);
        return initData(list, hotWordsSearchParams);
    }

    @Override
    public List<HotWordsHistory> queryByDay(Date queryTime) {
        QueryWrapper queryWrapper = new QueryWrapper();
        queryWrapper.eq("create_time", queryTime);
        return list(queryWrapper);
    }

    /**
     * 历史统计查询
     *
     * @param source
     * @return
     */
    private List<HotWordsHistory> initData(List<HotWordsHistory> source, HotWordsSearchParams hotWordsSearchParams) {

        //结果集
        List<HotWordsHistory> onlineMemberVOS = new ArrayList<>();
        //时间初始化
        Calendar calendar = Calendar.getInstance(TimeZone.getTimeZone("GMT-8:00"));
        calendar.setTime(hotWordsSearchParams.getStartTIme());
        //判定是否超出时间 如果结束时间在循环时间之后，则跳出循环
        while (hotWordsSearchParams.getEndTime().after(calendar.getTime())) {

//           //筛选时间相等的查询结果
            Optional<HotWordsHistory> first = source.stream().filter(item -> item.getCreateTime().equals(calendar.getTime())).findFirst();
            if (first.isPresent()) {
                onlineMemberVOS.add(first.get());
            } else {
                onlineMemberVOS.add(new HotWordsHistory(hotWordsSearchParams.getKeywords(), 0, calendar.getTime()));
            }
//            for (HotWordsHistory hotWordsHistory : source) {
//                System.out.println(hotWordsHistory.getCreateTime().getTime() + "-" + calendar.getTime().getTime());
//                if (hotWordsHistory.getCreateTime().equals(calendar.getTime())) {
//                    onlineMemberVOS.add(hotWordsHistory);
//                } else {
//                    onlineMemberVOS.add(new HotWordsHistory(hotWordsSearchParams.getKeywords(), 0, calendar.getTime()));
//                }
//            }

            calendar.set(Calendar.DAY_OF_MONTH, calendar.get(Calendar.DAY_OF_MONTH) + 1);
        }
        return onlineMemberVOS;
    }
}
