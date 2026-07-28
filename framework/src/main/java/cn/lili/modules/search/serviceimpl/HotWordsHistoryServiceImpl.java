package cn.lili.modules.search.serviceimpl;

import cn.lili.common.utils.DateUtil;
import cn.lili.modules.search.entity.dos.HotWordsHistory;
import cn.lili.modules.search.entity.dto.HotWordsSearchParams;
import cn.lili.modules.search.mapper.HotWordsHistoryMapper;
import cn.lili.modules.search.service.HotWordsHistoryService;
import cn.lili.modules.search.service.HotWordsService;
import cn.lili.modules.statistics.entity.enums.SearchTypeEnum;
import cn.lili.modules.statistics.util.StatisticsDateUtil;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
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

    @Autowired
    private HotWordsService hotWordsService;

    /**
     * 热词数量上限，避免前端传入超大值造成全表扫描
     */
    private static final int MAX_TOP = 500;

    private static final int DEFAULT_TOP = 50;

    @Override
    public List<HotWordsHistory> statistics(HotWordsSearchParams hotWordsSearchParams) {
        int top = limitTop(hotWordsSearchParams.getTop());
        if (SearchTypeEnum.TODAY.name().equals(hotWordsSearchParams.getSearchType())) {
            return hotWordsService.getHotWordsVO(top);
        }
        QueryWrapper queryWrapper = hotWordsSearchParams.queryWrapper();

        queryWrapper.groupBy("keywords");
        queryWrapper.orderByDesc("score");
        queryWrapper.last("limit " + top);
        List<HotWordsHistory> list = baseMapper.statistics(queryWrapper);
        return list;
    }

    private int limitTop(Integer top) {
        if (top == null || top <= 0) {
            return DEFAULT_TOP;
        }
        return Math.min(top, MAX_TOP);
    }

    @Override
    public List<HotWordsHistory> queryByDay(Date queryTime) {
        QueryWrapper queryWrapper = new QueryWrapper();

        Date[] dates = StatisticsDateUtil.getDateArray(queryTime);
        queryWrapper.between("create_time", dates[0], dates[1]);
        return list(queryWrapper);
    }

}
