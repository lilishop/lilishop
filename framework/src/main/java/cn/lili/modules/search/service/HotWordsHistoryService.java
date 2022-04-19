package cn.lili.modules.search.service;

import cn.lili.modules.search.entity.dos.CustomWords;
import cn.lili.modules.search.entity.dos.HotWordsHistory;
import cn.lili.modules.search.entity.dto.HotWordsDTO;
import cn.lili.modules.search.entity.dto.HotWordsSearchParams;
import cn.lili.modules.search.entity.vo.HotWordsHistoryVO;
import com.baomidou.mybatisplus.core.conditions.Wrapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import org.apache.poi.ss.formula.functions.T;

import java.util.Date;
import java.util.List;

/**
 * HotWordsService
 *
 * @author Chopper
 * @version v1.0
 * 2022-04-14 09:35
 */
public interface HotWordsHistoryService extends IService<HotWordsHistory> {

    /**
     * 热词统计
     *
     * @param hotWordsSearchParams
     * @return
     */
    List<HotWordsHistory> statistics(HotWordsSearchParams hotWordsSearchParams);

    /**
     * 根据时间查询
     *
     * @param queryTime 查询时间
     * @return
     */
    List<HotWordsHistory> queryByDay(Date queryTime);
}
