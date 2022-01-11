package cn.lili.modules.promotion.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanjiaActivityDTO;
import cn.lili.modules.promotion.entity.dto.search.KanJiaActivityLogQuery;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;


/**
 * 砍价活动日志业务层
 *
 * @author qiuqiu
 * @date 2021/7/1 9:45 上午
 */
public interface KanjiaActivityLogService extends IService<KanjiaActivityLog> {

    /**
     * 根据砍价参与记录id查询砍价记录
     *
     * @param kanJiaActivityLogQuery 砍价活动帮砍信息
     * @param pageVO                 分页信息
     * @return 砍价日志
     */
    IPage<KanjiaActivityLog> getForPage(KanJiaActivityLogQuery kanJiaActivityLogQuery, PageVO pageVO);

    /**
     * 砍一刀
     *
     * @param kanJiaActivityDTO 砍价记录
     * @return
     */
    KanjiaActivityLog addKanJiaActivityLog(KanjiaActivityDTO kanJiaActivityDTO);
}